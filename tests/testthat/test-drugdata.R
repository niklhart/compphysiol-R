# Unit tests for Drug and DrugList classes and related methods

test_that("Drug object are created successfully", {
    # Empty drug() case
    empty_drug <- drug()
    expect_s3_class(empty_drug, "Drug")
    expect_true(length(names(empty_drug$param)) == 0)

    # Drug with parameters
    drug <- drug(
        logP = 2.5,
        MW = 100[g / mol]
    )
    expect_setequal(drug$Parameter, c("logP", "MW"))

    skip("Accessing parameters from Drug objects not yet functional.")

    expect_equal(drug$param$logP, 2.5)
    expect_equal(drug$param$MW, units::set_units(100, "g/mol"))
})

test_that("Drug and Drugs objects interact correctly", {
    drug1 <- drug(logP = 2.5)
    drug2 <- drug(MW = 100[g / mol])
    drugs <- c(drug1, drug2)

    expect_s3_class(drugs, "Drugs")
    expect_equal(length(drugs), 2)
    expect_s3_class(drugs[1], "Drugs")
    expect_s3_class(drugs[[1]], "Drug")
    expect_s3_class(drugs[[2]], "Drug")

    skip("Subsetting Drugs objects not yet functional.")
    
    expect_equal(length(drugs[1]), 1)
})

test_that("Drug and Drugs objects can be printed without error", {

    skip("Drugs print method not functional currently.")

    drug <- drug(logP = 2.5, MW = 100[g / mol])
    expect_snapshot(print(drug))

    drugs <- c(A = drug, B = drug)
    expect_snapshot(print(drugs))
})

test_that("The param() accessors work correctly for Drug and Drugs objects", {

    skip("param() accessor not yet implemented for Drug objects")

    fuP_values <- c(human = 0.1, mouse = 0.2)

    # Check scalar and vector cases for Drug objects
    drug <- drug(logP = 2.5, MW = 100[g / mol], fuP = fuP_values)
    expect_equal(param(drug, "logP", simplify = FALSE), parameters(logP = 2.5))
    expect_equal(param(drug, "logP", simplify = TRUE), 2.5)
    expect_equal(param(drug, "fuP", simplify = FALSE), parameters(fuP = fuP_values))
    expect_equal(param(drug, "fuP", simplify = TRUE), fuP_values)
    expect_equal(param(drug, "fuP", species = "human", simplify = FALSE), parameters(fuP = fuP_values[['human']]))
    expect_equal(param(drug, "fuP", species = "human", simplify = TRUE), fuP_values[['human']])

    # Check scalar, vector and matrix cases for DrugList objects
    drug_list <- c(A = drug, B = drug)
    expect_equal(param(drug_list, drug = "A", name = "logP"), 2.5)
    expect_equal(param(drug_list, "logP"), c(A = 2.5, B = 2.5))

    expect_equal(param(drug_list, "fuP"), rbind(A = fuP_values, B = fuP_values))
})


test_that("Metadata can be specified via %...% tags", {

    skip("Metadata tags not yet implemented in drug() constructor")

    drug <- drug(
        logP = 2.5 %url% "drugbank.ca",
        pKa = 7.4 %def% "acid dissociation constant"
    )
    expect_equal(drug$meta$logP$url, "drugbank.ca")
    expect_equal(drug$meta$pKa$def, "acid dissociation constant")
})