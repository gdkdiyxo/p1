package ojt.management.controllers;


import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import ojt.management.business.services.AccountService;
import ojt.management.mappers.UserMapper;
import ojt.management.common.payload.dto.UserDTO;
import org.springframework.web.bind.annotation.*;

import java.sql.Timestamp;
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

    @GetMapping
    public List<UserDTO> getAllUsers() {
        return accountService.getAllUsers().stream().map(userMapper::userToUserDTO).collect(Collectors.toList());
    }

    @GetMapping("/{id}")
    public UserDTO getUserById(@PathVariable Long id) {
        return  userMapper.userToUserDTO(accountService.getUserById(id));
    }

    @GetMapping("/search")
    public List<UserDTO> searchUser(@RequestParam(value = "name", required = false) String name,
                                    @RequestParam(value = "email", required = false) String email,
                                    @RequestParam(value = "phone", required = false) String phone) {
        return accountService.searchUser(name, email, phone).stream().map(userMapper::userToUserDTO).collect(Collectors.toList());
    }

    @PutMapping("/update/{id}")
    public UserDTO updateUser(@RequestParam(value = "phone", required = false) String phone,
                              @RequestParam(value = "address", required = false) String address,
                              @RequestParam(value = "password", required = false) String password,
                              @RequestParam(value = "updateAt", required = false) Timestamp updateAt) {
        return userMapper.userToUserDTO(accountService.updateUser(phone, address, password, updateAt));
    }
}
