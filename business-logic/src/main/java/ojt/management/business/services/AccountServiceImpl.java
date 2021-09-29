package ojt.management.business.services;


import ojt.management.data.entities.Account;
import ojt.management.data.repositories.AccountRepository;
import ojt.management.common.payload.request.LoginRequest;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class AccountServiceImpl implements AccountService {

    private final AccountRepository accountRepository;

    public AccountServiceImpl(AccountRepository accountRepository) {
        this.accountRepository = accountRepository;
    }

    @Override
    public Account getUserById(Long id) {
        return accountRepository.getById(id);
    }

    @Override
    public List<Account> searchUser(String name, String email, String phone) { return accountRepository.searchUser(name, email, phone); }

    @Override
    public Account updateUser(Long id, String phone, String address, String password) {
            Account account = accountRepository.getById(id);
            account.setPhone(phone);
            account.getStudent().setAddress(address);
            account.setPassword(password);
            accountRepository.save(account);
            return accountRepository.getById(account.getId());
    }

    @Override
    public boolean deleteUser(Long id) {
        Account account = accountRepository.getById(id);
        boolean response=false;
        if (account != null || account.isDisabled()==false) {
            account.setDisabled(true);
            response = true;
            return response;
        }
        return response;
    }

}
