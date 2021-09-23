package com.ojt.management.services;

import com.ojt.management.entities.Account;
import com.ojt.management.repositories.AccountRepository;

import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class AccountServiceImpl implements AccountService {

    private final AccountRepository accountRepository;

    public AccountServiceImpl(AccountRepository accountRepository) {
        this.accountRepository = accountRepository;
    }

    @Override
    public List<Account> getAllUsers() {
        return accountRepository.findAll();
    }

    @Override
    public Optional<Account> getUserById(Long id) {
        return accountRepository.findById(id);
    }

    @Override
    public List<Account> searchUser(String name, String email, String phone) { return accountRepository.searchUser(name, email, phone); }

}
