package com.app.source;

import com.app.source.controllers.AuthController;
import com.app.source.entities.Role;
import com.app.source.enums.RoleEnum;
import com.app.source.exceptions.CompanyNotExistedException;
import com.app.source.exceptions.EmailAlreadyExistedException;
import com.app.source.exceptions.EmptyRoleException;
import com.app.source.exceptions.MajorNotExistedException;
import com.app.source.exceptions.UsernameAlreadyExistedException;
import com.app.source.payload.request.SignupRequest;
import com.app.source.repositories.AccountRepository;
import com.app.source.repositories.RoleRepository;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.List;

@Component
public class Seeder {

    private RoleRepository roleRepository;
    private AuthController authController;
    private AccountRepository accountRepository;


    public Seeder(RoleRepository roleRepository, AuthController authController, AccountRepository accountRepository) {
        this.roleRepository = roleRepository;
        this.authController = authController;
        this.accountRepository = accountRepository;
    }

    @EventListener
    private void seed(ApplicationReadyEvent event) {

        if (roleRepository.count() == 0) {
            seedRole();
        }
        if (accountRepository.count() == 0) {
            seedAccount();
        }
    }

    private void seedRole() {

        List<Role> roles = Arrays.asList(
                new Role(null, RoleEnum.SYS_ADMIN),
                new Role(null, RoleEnum.COMPANY_REPRESENTATIVE),
                new Role(null, RoleEnum.STUDENT));
        roleRepository.saveAll(roles);
    }

    private void seedAccount() {
        List<SignupRequest> signupRequests = Arrays.asList(
                new SignupRequest("thanhthu0321@gmail.com", "123456", "Thu", "Cao", "SYS_ADMIN", "0988388736"));

        signupRequests.stream().forEach(signupRequest -> {
            try {
                authController.registerUser(signupRequest);
            } catch (UsernameAlreadyExistedException | EmailAlreadyExistedException | EmptyRoleException | CompanyNotExistedException | MajorNotExistedException e) {
                e.printStackTrace();
            }
        });
    }
}
