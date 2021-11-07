package ojt.management.data.repositories;

import ojt.management.data.entities.Job;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface JobRepository extends JpaRepository<Job, Long> {
    //Search all job (for students)
    @Query("SELECT distinct j " +
            "FROM Job j " +
            "INNER JOIN j.semesters s " +
            "INNER JOIN j.majors m " +
            "WHERE j.name LIKE :name " +
            "AND j.title LIKE :title " +
            "AND (s.id = :semesterId OR :semesterId IS NULL) " +
            "AND (m.id = :majorId OR :majorId IS NULL)")
    List<Job> searchJob(@Param("name") String name, @Param("title") String title,
                        @Param("semesterId") Long semesterId, @Param("majorId") Long majorId);

    //Get job by ID for Rep
    @Query("SELECT DISTINCT j " +
            "FROM Job j " +
            "INNER JOIN j.company c " +
            "WHERE c.id = :companyId " +
            "AND j.id = :id")
    Job getJobByRep(@Param("companyId") Long companyId, @Param("id") Long id);

    //Search all job (for Rep)
    @Query("SELECT distinct j " +
            "FROM Job j " +
            "INNER JOIN j.semesters s " +
            "INNER JOIN j.majors m " +
            "INNER JOIN j.company c " +
            "WHERE j.name LIKE :name " +
            "AND j.title LIKE :title " +
            "AND (s.id = :semesterId OR :semesterId IS NULL) " +
            "AND (m.id = :majorId OR :majorId IS NULL) " +
            "AND c.id = :companyId")
    List<Job> searchJobByRep(@Param("name") String name,
                             @Param("title") String title,
                             @Param("semesterId") Long semesterId,
                             @Param("majorId") Long majorId,
                             @Param("companyId") Long companyId);

    List<Job> findAllByIdIn(List<Long> ids);
}
