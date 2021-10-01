package ojt.management.common.exceptions;

import org.springframework.http.HttpStatus;

public class AccountIdNotExistException extends CrudException {
    public AccountIdNotExistException() { super("Account ID dose not exist!", HttpStatus.BAD_REQUEST); }
}
