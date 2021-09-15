package com.app.source.repositories;

import com.app.source.entities.Account;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ApplicationUserRepository extends JpaRepository<Account, Long> {
    Account findByUsername(String username);

    Boolean existsByUsername(String username);

    Boolean existsByEmail(String email);
}
