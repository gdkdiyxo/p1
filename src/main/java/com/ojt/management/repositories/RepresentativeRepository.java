package com.ojt.management.repositories;

import com.ojt.management.entities.Representative;
import org.springframework.data.jpa.repository.JpaRepository;

public interface RepresentativeRepository extends JpaRepository<Representative, Long> {
}
