package com.app.source.entities;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.validation.constraints.Email;

@Entity
@Table(name = "application_user")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ApplicationUser {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;
    @Column(name = "username", unique = true)
    private String username;
    @Column(name = "password")
    private String password;

    @Email
    @Column(name = "email", unique = true)
    private String email;

    @OneToOne
    private Role role;

    public ApplicationUser(String username, String password, String email) {
        this.username = username;
        this.password = password;
        this.email = email;
    }
}
