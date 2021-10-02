package ojt.management.common.payload.request;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class CompanyUpdateRequest {
    private String name;
    private String description;
}
