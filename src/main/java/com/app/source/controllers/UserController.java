package com.app.source.controllers;

import com.app.source.mappers.UserMapper;
import com.app.source.payload.dto.UserDTO;
import com.app.source.services.AccountService;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import org.springframework.web.bind.annotation.*;

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
        return accountService.getAllUsers().stream().map(userMapper::userToUserDTO).collect(Collectors.toList());
    }

    @GetMapping("/{id}")
    public Optional<UserDTO> getUserById(@PathVariable Long id) {
        return accountService.getUserById(id).map(userMapper::userToUserDTO);
    }

    @GetMapping("/search")
    public List<UserDTO> searchUser(@RequestParam(value = "name", required = false) String name,
                                    @RequestParam(value = "email", required = false) String email,
                                    @RequestParam(value = "phone", required = false) String phone) {
        return accountService.searchUser(name, email, phone).stream().map(userMapper::userToUserDTO).collect(Collectors.toList());

    }
}
