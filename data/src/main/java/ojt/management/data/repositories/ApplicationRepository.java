package ojt.management.data.repositories;

import ojt.management.data.entities.Application;
import ojt.management.data.entities.Job;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.security.core.parameters.P;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ApplicationRepository extends JpaRepository<Application, Long> {

    @Query("SELECT a " +
            "FROM Application a " +
            "WHERE a.job.company.id = :companyId")
    List<Application> searchAppRep(@Param("companyId") Long companyId);

    @Query("SELECT a " +
            "FROM Application a " +
            "WHERE a.student.id = :studentId")
    List<Application> searchAppStudent(@Param("studentId") Long studentId);

    @Query("SELECT a " +
            "FROM Application a " +
            "WHERE a.job.company.id = :companyId AND a.id = :id")
    Application getAppRep(@Param("id") Long id, @Param("companyId") Long companyId);

    @Query("SELECT a " +
            "FROM Application a " +
            "WHERE a.student.id = :studentId AND a.id = :id")
    Application getAppStudent(@Param("id") Long id, @Param("studentId") Long studentId);

    @Query("SELECT distinct a " +
            "FROM Application a " +
            "INNER JOIN a.job j " +
            "WHERE j.name LIKE :name " +
            "AND j.title LIKE :title " +
            "AND j.company.id = :companyId")
    List<Application> searchAppByRep(@Param("name") String name,
                                     @Param("title") String title,
                                     @Param("companyId") Long companyId);

    @Query("SELECT distinct a " +
            "FROM Application a " +
            "INNER JOIN a.job j " +
            "INNER JOIN a.student s " +
            "WHERE j.name LIKE :name " +
            "AND j.title LIKE :title " +
            "AND s.id = :studentId")
    List<Application> searchAppByStu(@Param("name") String name,
                                     @Param("title") String title,
                                     @Param("studentId") Long studentId);
}
