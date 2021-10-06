package ojt.management.controllers;

import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import ojt.management.business.services.AccountService;
import ojt.management.common.exceptions.AccountIdNotExistedException;
import ojt.management.common.payload.request.AccountUpdateRequest;
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

<<<<<<< HEAD
    public UserController(AccountService accountService, UserMapper userMapper) {
=======
    public UserController(AccountService accountService,
                          UserMapper userMapper,
                          AccountRepository accountRepository) {
>>>>>>> develop
        this.accountService = accountService;
        this.userMapper = userMapper;
    }

    @GetMapping("/{id}")
<<<<<<< HEAD
    public UserDTO getUserById(@PathVariable Long id) throws AccountIdNotExistException {
        return userMapper.userToUserDTO(accountService.getUserById(id));
=======
    public UserDTO getUserById(@PathVariable Long id) throws AccountIdNotExistedException {
        if (Boolean.FALSE.equals(accountRepository.existsById(id))) {
            throw new AccountIdNotExistedException();
        } else {
            return userMapper.userToUserDTO(accountService.getUserById(id));
        }
>>>>>>> develop
    }

    @GetMapping()
    public List<UserDTO> searchUser(@RequestParam(value = "name", required = false) String name,
                                    @RequestParam(value = "email", required = false) String email,
                                    @RequestParam(value = "phone", required = false) String phone) {
        return accountService.searchUser(name, email, phone).stream().map(userMapper::userToUserDTO).collect(Collectors.toList());
    }

    @PutMapping("/{id}")
<<<<<<< HEAD
    public UserDTO updateUser(@Valid @RequestBody AccountUpdateRequest accountUpdateRequest) throws AccountIdNotExistException {
        return userMapper.userToUserDTO(accountService.updateUser(accountUpdateRequest.getId(),
                accountUpdateRequest.getPhone(), accountUpdateRequest.getAddress(), accountUpdateRequest.getPassword()));
=======
    public UserDTO updateUser(@Valid @RequestBody AccountUpdateRequest accountUpdateRequest) {
        return userMapper.userToUserDTO(accountService.updateUser(
                accountUpdateRequest.getId(),
                accountUpdateRequest.getPhone(),
                accountUpdateRequest.getAddress(),
                accountUpdateRequest.getPassword()));
>>>>>>> develop
    }

    @DeleteMapping("/{id}")
    public boolean deleteUser(@PathVariable Long id) throws AccountIdNotExistException {
        return accountService.deleteUser(id);
    }
}
