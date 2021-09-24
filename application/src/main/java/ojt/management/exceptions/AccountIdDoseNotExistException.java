package ojt.management.exceptions;

import ojt.management.common.exceptions.CrudException;
import org.springframework.http.HttpStatus;

public class AccountIdDoseNotExistException extends CrudException {
    public AccountIdDoseNotExistException() { super("Account ID dose not exist!", HttpStatus.BAD_REQUEST); }
}
