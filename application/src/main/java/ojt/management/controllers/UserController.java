package ojt.management.controllers;

import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import ojt.management.business.services.AccountService;
import ojt.management.common.exceptions.AccountIdNotExistedException;
import ojt.management.common.payload.request.AccountUpdateRequest;
import ojt.management.data.repositories.AccountRepository;
import ojt.management.mappers.UserMapper;
import ojt.management.common.payload.dto.UserDTO;
import org.springframework.security.access.prepost.PostAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;
import java.util.stream.Collectors;

@RestController
@PostAuthorize("hasAnyAuthority('SYS_ADMIN')")
@RequestMapping("/users")
@SecurityRequirement(name = "bearerAuth")
public class UserController {

    private final AccountService accountService;
    private final UserMapper userMapper;
    private final AccountRepository accountRepository;

    public UserController(AccountService accountService, UserMapper userMapper, AccountRepository accountRepository) {
        this.accountService = accountService;
        this.userMapper = userMapper;
        this.accountRepository = accountRepository;
    }

    @GetMapping("/{id}")
    public UserDTO getUserById(@PathVariable Long id) throws AccountIdNotExistedException {
        if (Boolean.FALSE.equals(accountRepository.existsById(id))) {
            throw new AccountIdNotExistedException();
        } else {
            return userMapper.userToUserDTO(accountService.getUserById(id));
        }
    }

    @GetMapping()
    public List<UserDTO> searchUser(@RequestParam(value = "name", required = false) String name,
                                    @RequestParam(value = "email", required = false) String email,
                                    @RequestParam(value = "phone", required = false) String phone) {
        return accountService.searchUser(name, email, phone).stream().map(userMapper::userToUserDTO).collect(Collectors.toList());
    }

    @PutMapping("/{id}")
    public UserDTO updateUser(@Valid @RequestBody AccountUpdateRequest accountUpdateRequest) {
        return userMapper.userToUserDTO(accountService.updateUser(accountUpdateRequest.getId(),
                accountUpdateRequest.getPhone(), accountUpdateRequest.getAddress(), accountUpdateRequest.getPassword()));
    }

    @DeleteMapping("/{id}")
    public boolean deleteUser(@PathVariable Long id) {
        return accountService.deleteUser(id);
    }
}
