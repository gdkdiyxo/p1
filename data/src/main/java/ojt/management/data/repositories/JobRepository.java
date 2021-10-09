package ojt.management.data.repositories;

import ojt.management.data.entities.Job;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface JobRepository extends JpaRepository<Job, Long> {

    @Query("SELECT distinct j " +
            "FROM Job j " +
            "inner join j.semesters s " +
            "inner join j.majors m " +
            "WHERE j.name like :name " +
            "AND j.title like :title " +
            "AND (s.id = :semesterId or :semesterId is null) " +
            "AND (m.id = :majorId or :majorId is null)")
    List<Job> searchJob(@Param("name") String name, @Param("title") String title,
                        @Param("semesterId") Long semesterId, @Param("majorId") Long majorId);

    boolean existsByName(String name);

}
