package ojt.management.business.services;

import ojt.management.data.entities.Account;

import java.util.List;

public interface AccountService {
    Account getUserById(Long id);

    List<Account> searchUser(String name, String email, String phone);

    Account updateUser(String phone, String address, String password);

    boolean deleteUser(Long id);
}
