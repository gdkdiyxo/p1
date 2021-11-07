package ojt.management.data.repositories;

import ojt.management.data.entities.Evaluation;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface EvaluationRepository extends JpaRepository<Evaluation, Long>, JpaSpecificationExecutor<Evaluation> {
    @Query("select e " +
            "FROM Evaluation e " +
            "where e.application.student.id = :studentId and e.id = :id")
    Evaluation getEvaluationStudent(@Param("studentId") Long studentId,@Param("id") Long id);

    @Query("select e " +
            "FROM Evaluation e " +
            "where e.application.job.company.id = :companyId and e.id = :id")
    Evaluation getEvaluationRep(@Param("companyId") Long companyId,@Param("id") Long id);
}
