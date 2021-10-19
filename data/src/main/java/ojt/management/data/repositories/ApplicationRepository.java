package ojt.management.data.repositories;

import ojt.management.data.entities.Application;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ApplicationRepository extends JpaRepository<Application, Long> {

    @Query("select a FROM Application a where a.job.company.id = :companyId")
    List<Application> searchAppRep(@Param("companyId") Long companyId);

    @Query("select a FROM Application a where a.student.id = :studentId")
    List<Application> searchAppStudent(@Param("studentId") Long studentId);

    @Query("select a FROM Application a where a.job.company.id = :companyId and a.id = :id")
    Application getAppRep(@Param("id") Long id, @Param("companyId") Long companyId);

    @Query("select a FROM Application a where a.student.id = :studentId and a.id = :id")
    Application getAppStudent(@Param("id") Long id, @Param("studentId") Long studentId);
}
