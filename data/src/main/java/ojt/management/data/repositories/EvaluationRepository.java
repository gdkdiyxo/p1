package ojt.management.data.repositories;

import ojt.management.data.entities.Evaluation;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface EvaluationRepository extends JpaRepository<Evaluation, Long> {
    @Query("select e " +
            "FROM Evaluation e " +
            "where e.application.student.id = :studentId")
    List<Evaluation> searchEvaluationStudent(@Param("studentId") Long studentId);

    @Query("select e " +
            "FROM Evaluation e " +
            "where e.application.student.studentCode = :studentCode and e.application.job.company.id = :companyId")
    List<Evaluation> searchEvaluationRep(@Param("companyId") Long companyId, @Param("studentCode") String studentCode);

    @Query("select e " +
            "FROM Evaluation e " +
            "where e.application.student.id = :studentId and e.id = :id")
    Evaluation getEvaluationStudent(@Param("studentId") Long studentId,@Param("id") Long id);

    @Query("select e " +
            "FROM Evaluation e " +
            "where e.application.job.company.id = :companyId and e.id = :id")
    Evaluation getEvaluationRep(@Param("companyId") Long companyId,@Param("id") Long id);
}
