package com.app.source.payload.response;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class ServicesMegaDepleteDTO implements Serializable {
    String erpSchema;
    String plantCode;
    String partNumber;
    String demandPlantSubBusiness;
    String demandPlantRegion;
    String demandPlantCountry;
    String demandPlantCategory;
    String demandPlantCode;
    int demandPlantQty;
    String demandPlantPartDescription;
    double demandPlantWeight;
    String demandPlantBuyerName;
    String demandPlantPlannerName;
    double partOppValueUsd;
    double opportunityValueUsd;
    String excessPlantSubBusiness;
    String excessPlantRegion;
    String excessPlantCountry;
    String excessPlantCategory;
    String excessPlantCode;
    int excessPlantQty;
    String excessPlantSubInventoryQty;
    double excessPlantCostLc;
    double excessPlantCostUsd;
    double excessPlantCostLc5Percent;
    double excessPlantCostUsd5Percent;
    String excessPlantBuyerName;
    String excessPlantPlannerName;
    String currency;
    int priorityRule;
    int actionId;
    String actionDescription;
    String actionCreatedBy;
    String actionUpdatedBy;
}
