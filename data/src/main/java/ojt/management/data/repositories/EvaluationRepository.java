package ojt.management.data.repositories;

import ojt.management.data.entities.Evaluation;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface EvaluationRepository extends JpaRepository<Evaluation, Long> {
    @Query("SELECT e " +
            "FROM Evaluation e " +
            "WHERE e.application.student.id = :studentId")
    List<Evaluation> searchEvaluationStudent(@Param("studentId") Long studentId);

    @Query("SELECT e " +
            "FROM Evaluation e " +
            "WHERE e.application.student.studentCode = :studentCode " +
            "AND e.application.job.company.id = :companyId")
    List<Evaluation> searchEvaluationRep(@Param("companyId") Long companyId, @Param("studentCode") String studentCode);

    @Query("SELECT e " +
            "FROM Evaluation e " +
            "WHERE e.application.student.id = :studentId " +
            "AND e.id = :id")
    Evaluation getEvaluationStudent(@Param("studentId") Long studentId, @Param("id") Long id);

    @Query("SELECT e " +
            "FROM Evaluation e " +
            "WHERE e.application.job.company.id = :companyId " +
            "AND e.id = :id")
    Evaluation getEvaluationRep(@Param("companyId") Long companyId, @Param("id") Long id);
}
