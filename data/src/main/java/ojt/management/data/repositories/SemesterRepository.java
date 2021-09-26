package ojt.management.data.repositories;

import ojt.management.data.entities.Major;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Date;

public interface SemesterRepository {

    @Query("SELECT a FROM Semester a where a.name = :name or a.startDate = :startDate or a.endDate = :endDate")
    Major searchSemester(@Param("name") String name, @Param("startDate") Date startDate, @Param("endDate") Date endDate);
}
