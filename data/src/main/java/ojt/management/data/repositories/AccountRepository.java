package ojt.management.data.repositories;

import com.querydsl.core.types.dsl.StringPath;
import ojt.management.data.entities.Account;
import ojt.management.data.entities.QAccount;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.data.querydsl.binding.QuerydslBinderCustomizer;
import org.springframework.data.querydsl.binding.QuerydslBindings;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface AccountRepository extends JpaRepository<Account, Long>, QuerydslPredicateExecutor<Account>, QuerydslBinderCustomizer<QAccount> {
    @Override
    default void customize(QuerydslBindings bindings, QAccount root) {
        bindings.bind(String.class).first(
                (StringPath path, String value) -> path.containsIgnoreCase(value));
    }

    Account findByEmail(String email);

    @Query("SELECT a " +
            "FROM Account a " +
            "WHERE a.name like :name " +
            "OR a.email = :email " +
            "OR a.phone = :phone ")
    List<Account> searchUser(@Param("name") String name, @Param("email") String email, @Param("phone") String phone);

    Boolean existsByStudent_StudentCode(String studentCode);

    Boolean existsByEmail(String email);

    boolean existsById(Long id);

}
