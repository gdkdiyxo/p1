package com.app.source.entities;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.io.Serializable;

@Entity
@Table(name = "representative")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class Representative implements Serializable {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    //----------[Start]Mapping relationship----------
    @OneToOne(mappedBy = "representative")
    private Company company;

    @OneToOne(mappedBy = "representative")
    private Account account;
    //----------[End]Mapping relationship----------

}
