package com.app.source.services;

import com.app.source.entities.ApplicationUser;
import com.app.source.repositories.ApplicationUserRepository;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class ApplicationUserServiceImpl implements ApplicationUserService {

    private final ApplicationUserRepository applicationUserRepository;

    public ApplicationUserServiceImpl(ApplicationUserRepository applicationUserRepository) {
        this.applicationUserRepository = applicationUserRepository;
    }

    @Override
    public List<ApplicationUser> getAllUsers() {
        return applicationUserRepository.findAll();
    }
}
