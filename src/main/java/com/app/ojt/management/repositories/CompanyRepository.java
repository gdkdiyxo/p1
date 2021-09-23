package com.app.ojt.management.repositories;

import com.app.ojt.management.entities.Company;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CompanyRepository extends JpaRepository<Company, Long> {
}
