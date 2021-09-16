package com.app.source.entities;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;
import java.util.Date;

@Entity
@Table(name = "application")
@Data
@AllArgsConstructor
@NoArgsConstructor
@EntityListeners(AuditingEntityListener.class)

public class Application {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "student_id")
    private Long studentId;

    @Column(name = "job_id")
    private Long jobId;

    @Column(name = "experience", length=1000)
    private String experience;

    @Column(name = "cv_key", nullable = false)
    private String cvKey;

    @Column(name = "is_company_accepted")
    private boolean isCompanyAccepted;

    @Column(name = "acceptedd_at")
    private Date acceptedAt;

    @Column(name = "is_student_comfirmed")
    private boolean isStudentComfirmed;

    @Column(name = "comfirmed_at")
    private Date comfirmeddAt;

    @OneToOne
    private Evaluation evaluation;

    @ManyToOne
    private Student student;

    @ManyToOne
    private Job job;

    @CreatedDate
    @Column(name = "created_at")
    private Date createdAt;

    @Column(name = "is_removed")
    private boolean isRemoved;

    public Application(String experience, String cvKey){
        this.experience = experience;
        this.cvKey = cvKey;
    }

}
