package ojt.management.controllers;

import cz.jirutka.rsql.parser.RSQLParser;
import cz.jirutka.rsql.parser.ast.Node;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import ojt.management.business.services.AccountService;
import ojt.management.common.exceptions.AccountIdNotExistedException;
import ojt.management.common.payload.request.AccountUpdateRequest;
import ojt.management.configuration.security.services.UserDetailsImpl;
import ojt.management.data.entities.Account;
import ojt.management.data.rsql.CustomRsqlVisitor;
import ojt.management.mappers.UserMapper;
import ojt.management.common.payload.dto.UserDTO;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.security.access.prepost.PostAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/users")
@SecurityRequirement(name = "bearerAuth")
public class UserController {

    private final AccountService accountService;
    private final UserMapper userMapper;

    public UserController(AccountService accountService, UserMapper userMapper) {
        this.accountService = accountService;
        this.userMapper = userMapper;
    }

    @PostAuthorize("hasAnyAuthority('SYS_ADMIN')")
    @GetMapping("/{id}")
    public UserDTO getUserById(@PathVariable Long id)
            throws AccountIdNotExistedException {
        return userMapper.userToUserDTO(accountService.getUserById(id));
    }

    @PostAuthorize("hasAnyAuthority('SYS_ADMIN')")
    @GetMapping()
    public List<UserDTO> searchUser(@RequestParam(value = "search") String search) {
        Node rootNode = new RSQLParser().parse(search);
        Specification<Account> spec = rootNode.accept(new CustomRsqlVisitor<>());
        return accountService.searchUser(spec).stream().map(userMapper::userToUserDTO).collect(Collectors.toList());
    }

    @PostAuthorize("hasAnyAuthority('COMPANY_REPRESENTATIVE','SYS_ADMIN', 'STUDENT')")
    @PutMapping("/{id}")
    public UserDTO updateUser(@Valid @RequestBody AccountUpdateRequest accountUpdateRequest, Authentication authentication)
            throws AccountIdNotExistedException {
        Long accountId = ((UserDetailsImpl) authentication.getPrincipal()).getId();
        return userMapper.userToUserDTO(accountService.updateUser(accountUpdateRequest,accountId));
    }

    @PostAuthorize("hasAnyAuthority('SYS_ADMIN')")
    @DeleteMapping("/{id}")
    public boolean deleteUser(@PathVariable Long id)
            throws AccountIdNotExistedException {
        return accountService.deleteUser(id);
    }
}
