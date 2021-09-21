package com.app.source.services;

import com.app.source.entities.Account;
import org.springframework.data.domain.Sort;

import java.util.List;
import java.util.Optional;

public interface AccountService {
    List<Account> getAllUsers();

    Optional<Account> getUserById(Long id);

    List<Account> searchUser(String name, String email, String phone);
}
