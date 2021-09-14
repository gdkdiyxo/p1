package com.app.source.repositories;

import com.app.source.entities.Role;
import com.app.source.enums.RoleEnum;
import org.springframework.data.jpa.repository.JpaRepository;

public interface RoleRepository extends JpaRepository<Role, Integer> {
    Role findByName(RoleEnum name);
}
