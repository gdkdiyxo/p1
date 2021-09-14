package com.app.source.controllers;

import com.app.source.configuration.security.services.UserDetailsImpl;
import com.app.source.entities.ApplicationUser;
import com.app.source.entities.Role;
import com.app.source.enums.RoleEnum;
import com.app.source.exceptions.CrudException;
import com.app.source.exceptions.EmailAlreadyExistedException;
import com.app.source.exceptions.UsernameAlreadyExistedException;
import com.app.source.payload.request.LoginRequest;
import com.app.source.payload.request.SignupRequest;
import com.app.source.payload.response.DataResponse;
import com.app.source.payload.response.JwtResponse;
import com.app.source.payload.response.Response;
import com.app.source.repositories.ApplicationUserRepository;
import com.app.source.repositories.RoleRepository;
import com.app.source.utils.JwtUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@CrossOrigin(origins = "*", maxAge = 3600)
@RestController
@RequestMapping("/api/auth")
public class AuthController {
    private final AuthenticationManager authenticationManager;

    private final ApplicationUserRepository userRepository;

    private final RoleRepository roleRepository;

    private final PasswordEncoder encoder;

    private final JwtUtils jwtUtils;

    public AuthController(AuthenticationManager authenticationManager, ApplicationUserRepository userRepository,
                          RoleRepository roleRepository, PasswordEncoder encoder, JwtUtils jwtUtils) {
        this.authenticationManager = authenticationManager;
        this.userRepository = userRepository;
        this.roleRepository = roleRepository;
        this.encoder = encoder;
        this.jwtUtils = jwtUtils;

    }

    @PostMapping("/signin")
    public ResponseEntity<JwtResponse> authenticateUser(@Valid @RequestBody LoginRequest loginRequest) {

        Authentication authentication = authenticationManager.authenticate(
                new UsernamePasswordAuthenticationToken(loginRequest.getUsername(), loginRequest.getPassword()));

        SecurityContextHolder.getContext().setAuthentication(authentication);
        String jwt = jwtUtils.generateJwtToken(authentication);

        UserDetailsImpl userDetails = (UserDetailsImpl) authentication.getPrincipal();
        List<String> roles = userDetails.getAuthorities().stream()
                .map(GrantedAuthority::getAuthority)
                .collect(Collectors.toList());

        return ResponseEntity.ok(new JwtResponse(jwt,
                userDetails.getId(),
                userDetails.getUsername(),
                userDetails.getEmail(),
                roles));
    }

    @PostMapping("/signup")
    public ResponseEntity<DataResponse<ApplicationUser>> registerUser(@Valid @RequestBody SignupRequest signUpRequest) throws UsernameAlreadyExistedException, EmailAlreadyExistedException {
        RuntimeException runtimeException = new RuntimeException("Error: Role is not found.");
        if (Boolean.TRUE.equals(userRepository.existsByUsername(signUpRequest.getUsername()))) {
            throw new UsernameAlreadyExistedException();
        }

        if (Boolean.TRUE.equals(userRepository.existsByEmail(signUpRequest.getEmail()))) {
            throw new EmailAlreadyExistedException();
        }

        // Create new user's account
        ApplicationUser user = new ApplicationUser(signUpRequest.getUsername(),
                encoder.encode(signUpRequest.getPassword()),
                signUpRequest.getEmail());

        String strRole = signUpRequest.getRole();
        Role role;
        if (strRole == null) {
            role = Optional.ofNullable(roleRepository.findByName(RoleEnum.STUDENT))
                    .orElseThrow(() -> runtimeException);
        } else {
            switch (strRole) {
                case "SYS_ADMIN":
                    role = Optional.ofNullable(roleRepository.findByName(RoleEnum.SYS_ADMIN))
                            .orElseThrow(() -> runtimeException);
                    break;
                case "COMPANY_REPRESENTATIVE":
                    role = Optional.ofNullable(roleRepository.findByName(RoleEnum.COMPANY_REPRESENTATIVE))
                            .orElseThrow(() -> runtimeException);
                    break;
                default:
                    role = Optional.ofNullable(roleRepository.findByName(RoleEnum.STUDENT))
                            .orElseThrow(() -> runtimeException);
                    break;
            }
        }
        user.setRole(role);
        user = userRepository.save(user);
        return ResponseEntity.ok(new DataResponse<>("OK", "User registered successfully!", user));
    }

    @GetMapping("/error-example/{error}")
    public ResponseEntity<Response> errorMethod(@PathVariable boolean error) throws CrudException {
        if (error) {
            throw new CrudException("My custom error", HttpStatus.INTERNAL_SERVER_ERROR);
        }
        return ResponseEntity.ok(new Response("OK", "OK"));
    }
}
