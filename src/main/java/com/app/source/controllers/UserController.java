package com.app.source.controllers;

import com.app.source.mappers.UserMapper;
import com.app.source.payload.dto.UserDTO;
import com.app.source.services.ApplicationUserService;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/users")
@SecurityRequirement(name = "bearerAuth")
public class UserController {

    private final ApplicationUserService applicationUserService;
    private final UserMapper userMapper;

    public UserController(ApplicationUserService applicationUserService, UserMapper userMapper) {
        this.applicationUserService = applicationUserService;
        this.userMapper = userMapper;
    }

    @GetMapping
    public List<UserDTO> getAllUsers() {
        return applicationUserService.getAllUsers().stream().map(userMapper::userToUserDTO).collect(Collectors.toList());
    }

}
