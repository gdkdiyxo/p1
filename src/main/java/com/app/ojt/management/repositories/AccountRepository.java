package com.app.ojt.management.repositories;

import com.app.ojt.management.entities.Account;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface AccountRepository extends JpaRepository<Account, Long> {
    Account findByEmail(String email);

    @Query("SELECT a FROM Account a where a.name = :name")
    List<Account> findByName(@Param("name") String name);

    Boolean existsByStudent_StudentCode(String studentCode);

    Boolean existsByEmail(String email);
}
