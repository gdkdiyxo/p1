package com.app.ojt.management.controllers;

import com.app.ojt.management.mappers.UserMapper;
import com.app.ojt.management.payload.dto.UserDTO;
import com.app.ojt.management.services.AccountService;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Optional;
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
        accountService.getAllUsers().stream().forEach(user -> {
            System.out.println(user.getName());
        });
        return accountService.getAllUsers().stream().map(userMapper::userToUserDTO).collect(Collectors.toList());
    }

    @GetMapping("/{id}")
    public Optional<UserDTO> getUserById(@PathVariable Long id) {
        return accountService.getUserById(id).map(userMapper::userToUserDTO);
    }

//    @GetMapping("/search")
//    public List<UserDTO> searchUser(@RequestParam(value = "name", required = false) String name,
//                                    @RequestParam(value = "age", required = false) Integer age,
//                                    @RequestParam(value = "role", required = false) String role) {
//        return accountService.search(name, age, role);
//    }
}
