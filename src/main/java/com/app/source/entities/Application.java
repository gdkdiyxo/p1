package com.app.source.entities;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Timestamp;

@Entity
@Table(name = "application")
@Data
@AllArgsConstructor
@NoArgsConstructor
@EntityListeners(AuditingEntityListener.class)

public class Application implements Serializable {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "experience", length = 1000)
    private String experience;

    @Column(name = "cv_key", nullable = false)
    private String cvKey;

    @Column(name = "is_company_accepted")
    private boolean isCompanyAccepted;

    @Column(name = "accepted_at")
    private Timestamp acceptedAt;

    @Column(name = "is_student_comfirmed")
    private boolean isStudentComfirmed;

    @Column(name = "comfirmed_at")
    private Timestamp comfirmedAt;

    //----------[Start]Mapping relationship----------
    @OneToOne(mappedBy = "evaluation")
    private Evaluation evaluation;

    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false)
    private Student student;

    @ManyToOne
    @JoinColumn(name = "job_id", nullable = false)
    private Job job;
    //----------[End]Mapping relationship----------

    @CreatedDate
    @Column(name = "created_at", nullable = false)
    private Timestamp createdAt;

    @LastModifiedDate
    @Column(name = "updated_at")
    private Date updatedAt;

    @Column(name = "is_disabled")
    private boolean disabled;

    public Application(String experience, String cvKey){
        this.experience = experience;
        this.cvKey = cvKey;
    }

}
