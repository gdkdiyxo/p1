package ojt.management.data.entities;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Timestamp;
import java.util.HashSet;
import java.util.Set;

@Entity
@Table(name = "job")
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@EntityListeners(AuditingEntityListener.class)

public class Job implements Serializable {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "description", length = 2000, nullable = false)
    private String description;

    @Column(name = "title", nullable = false)
    private String title;

    @CreatedDate
    @Column(name = "created_at", nullable = false)
    private Timestamp createdAt;

    @LastModifiedDate
    @Column(name = "updated_at")
    private Timestamp updatedAt;

    @Column(name = "is_disabled", columnDefinition = "BOOLEAN DEFAULT false")
    private boolean disabled;

    //----------[Start]Mapping relationship----------
    @ManyToOne
    @JoinColumn(name = "company_id", nullable = false)
    private Company company;

    @ManyToMany(mappedBy = "jobs")
    private Set<Major> majors = new HashSet<>();

    @ManyToMany(mappedBy = "jobs")
    private Set<Semester> semesters;

    @OneToMany(mappedBy = "job")
    private Set<Application> applications;
    //----------[End]Mapping relationship----------

    public Job(String name, String description, String title) {
        this.name = name;
        this.description = description;
        this.title = title;
    }

    public Job(Long id) {
        this.id = id;
    }
}
