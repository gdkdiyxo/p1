package com.app.source.entities;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.io.Serializable;

@Entity
@Table(name = "student")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class Student implements Serializable {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "address", length = 512)
    private String address;

    @Column(name = "student_code", length = 10, nullable = false)
    private String studentCode;

    //----------[Start]Mapping relationship----------
    @OneToOne(mappedBy = "student")
    private Account account;

    @ManyToOne
    @JoinColumn(name = "major_id", nullable = false)
    private Major major;

    @ManyToOne
    @JoinColumn(name = "semester_id", nullable = false)
    private Semester semester;
    //----------[End]Mapping relationship----------

}
