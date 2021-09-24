package ojt.management.business.services;

import ojt.management.data.entities.Account;

import java.sql.Timestamp;
import java.util.List;

public interface AccountService {
    List<Account> getAllUsers();

    Account getUserById(Long id);

    List<Account> searchUser(String name, String email, String phone);

    Account updateUser(String phone, String address, String password, Timestamp updateAt);
}
