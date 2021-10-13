package ojt.management.business.services;


import ojt.management.common.exceptions.AccountIdNotExistedException;
import ojt.management.common.payload.request.AccountUpdateRequest;
import ojt.management.data.entities.Account;
import org.springframework.data.jpa.domain.Specification;

import java.util.List;

public interface AccountService {
    Account getUserById(Long id) throws AccountIdNotExistedException;

    List<Account> searchUser(Specification<Account> specification);

    Account updateUser(AccountUpdateRequest accountUpdateRequest, Long accountId) throws AccountIdNotExistedException;

    boolean deleteUser(Long id) throws AccountIdNotExistedException;
}
