package com.app.ojt.management.services;

import com.app.ojt.management.entities.Account;

import java.util.List;
import java.util.Optional;

public interface AccountService {
    List<Account> getAllUsers();

    Optional<Account> getUserById(Long id);
}
