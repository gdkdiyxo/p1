package ojt.management.controllers;

import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import ojt.management.business.services.EvaluationService;
import ojt.management.common.exceptions.AccountIdNotExistedException;
import ojt.management.common.exceptions.EvaluationIdNotExistedException;
import ojt.management.common.exceptions.NotPermissionException;
import ojt.management.common.payload.dto.EvaluationDTO;
import ojt.management.common.payload.request.EvaluationCreateRequest;
import ojt.management.common.payload.request.EvaluationUpdateRequest;
import ojt.management.configuration.security.services.UserDetailsImpl;
import ojt.management.mappers.EvaluationMapper;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/evaluations")
@SecurityRequirement(name = "bearerAuth")
public class EvaluationController {

    private final EvaluationService evaluationService;
    private final EvaluationMapper evaluationMapper;

    public EvaluationController(EvaluationMapper evaluationMapper,
                                EvaluationService evaluationService) {
        this.evaluationMapper = evaluationMapper;
        this.evaluationService = evaluationService;
    }

    @PreAuthorize("hasAnyAuthority('COMPANY_REPRESENTATIVE', 'STUDENT')")
    @GetMapping()
    public List<EvaluationDTO> searchEvaluation(@PathVariable String studentCode,
                                                Authentication authentication) throws AccountIdNotExistedException {
        Long accountId = ((UserDetailsImpl) authentication.getPrincipal()).getId();
        return evaluationService.searchEvaluation(studentCode, accountId).stream().map(
                evaluationMapper::evaluationToEvaluationDTO).collect(Collectors.toList());
    }

    @PreAuthorize("hasAnyAuthority('COMPANY_REPRESENTATIVE', 'STUDENT')")
    @GetMapping("/{id}")
    public EvaluationDTO getEvaluationById(@PathVariable Long id,
                                           Authentication authentication)
            throws EvaluationIdNotExistedException, AccountIdNotExistedException {
        Long accountId = ((UserDetailsImpl) authentication.getPrincipal()).getId();
        return evaluationMapper.evaluationToEvaluationDTO(evaluationService.getEvaluationById(id, accountId));
    }

    @PreAuthorize("hasAnyAuthority('COMPANY_REPRESENTATIVE')")
    @PutMapping("/{id}")
    public EvaluationDTO updateEvaluation(@PathVariable Long id,
                                          @RequestBody @Valid EvaluationUpdateRequest evaluationUpdateRequest,
                                          Authentication authentication)
            throws EvaluationIdNotExistedException, AccountIdNotExistedException {
        Long accountId = ((UserDetailsImpl) authentication.getPrincipal()).getId();
        return evaluationMapper.evaluationToEvaluationDTO(
                evaluationService.updateEvaluation(id, evaluationUpdateRequest, accountId));
    }

    @PreAuthorize("hasAnyAuthority('COMPANY_REPRESENTATIVE')")
    @PostMapping()
    public EvaluationDTO createEvaluation(@RequestBody @Valid EvaluationCreateRequest evaluationCreateRequest,
                                          Authentication authentication) throws NotPermissionException {
        Long accountId = ((UserDetailsImpl) authentication.getPrincipal()).getId();
        return evaluationMapper.evaluationToEvaluationDTO(
                evaluationService.createEvaluation(evaluationCreateRequest, accountId));
    }
}
