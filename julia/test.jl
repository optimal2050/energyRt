# eqTechSng2Sng(tech, region, comm, commp, year, slice)$meqTechSng2Sng(tech, region, comm, commp, year, slice)
print("eqTechSng2Sng(tech, region, comm, commp, year, slice)...")
@constraint(
    model,
    [(t, r, c, cp, y, s) in meqTechSng2Sng],
    vTechInp[(t, c, r, y, s)] * (
        if haskey(pTechCinp2use, (t, c, r, y, s))
            pTechCinp2use[(t, c, r, y, s)]
        else
            pTechCinp2useDef
        end
    ) ==
    (vTechOut[(t, cp, r, y, s)]) / (
        (
            if haskey(pTechUse2cact, (t, cp, r, y, s))
                pTechUse2cact[(t, cp, r, y, s)]
            else
                pTechUse2cactDef
            end
        ) * (
            if haskey(pTechCact2cout, (t, cp, r, y, s))
                pTechCact2cout[(t, cp, r, y, s)]
            else
                pTechCact2coutDef
            end
        )
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechGrp2Sng(tech, region, group, commp, year, slice)$meqTechGrp2Sng(tech, region, group, commp, year, slice)
print("eqTechGrp2Sng(tech, region, group, commp, year, slice)...")
@constraint(
    model,
    [(t, r, g, cp, y, s) in meqTechGrp2Sng],
    (
        if haskey(pTechGinp2use, (t, g, r, y, s))
            pTechGinp2use[(t, g, r, y, s)]
        else
            pTechGinp2useDef
        end
    ) * sum(
        (
            if (t, c, r, y, s) in mvTechInp
                (
                    vTechInp[(t, c, r, y, s)] * (
                        if haskey(pTechCinp2ginp, (t, c, r, y, s))
                            pTechCinp2ginp[(t, c, r, y, s)]
                        else
                            pTechCinp2ginpDef
                        end
                    )
                )
            else
                0
            end
        ) for c in comm if (t, g, c) in mTechGroupComm
    ) ==
    (vTechOut[(t, cp, r, y, s)]) / (
        (
            if haskey(pTechUse2cact, (t, cp, r, y, s))
                pTechUse2cact[(t, cp, r, y, s)]
            else
                pTechUse2cactDef
            end
        ) * (
            if haskey(pTechCact2cout, (t, cp, r, y, s))
                pTechCact2cout[(t, cp, r, y, s)]
            else
                pTechCact2coutDef
            end
        )
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechSng2Grp(tech, region, comm, groupp, year, slice)$meqTechSng2Grp(tech, region, comm, groupp, year, slice)
print("eqTechSng2Grp(tech, region, comm, groupp, year, slice)...")
@constraint(
    model,
    [(t, r, c, gp, y, s) in meqTechSng2Grp],
    vTechInp[(t, c, r, y, s)] * (
        if haskey(pTechCinp2use, (t, c, r, y, s))
            pTechCinp2use[(t, c, r, y, s)]
        else
            pTechCinp2useDef
        end
    ) == sum(
        (
            if (t, cp, r, y, s) in mvTechOut
                (
                    (vTechOut[(t, cp, r, y, s)]) / (
                        (
                            if haskey(pTechUse2cact, (t, cp, r, y, s))
                                pTechUse2cact[(t, cp, r, y, s)]
                            else
                                pTechUse2cactDef
                            end
                        ) * (
                            if haskey(pTechCact2cout, (t, cp, r, y, s))
                                pTechCact2cout[(t, cp, r, y, s)]
                            else
                                pTechCact2coutDef
                            end
                        )
                    )
                )
            else
                0
            end
        ) for cp in comm if (t, gp, cp) in mTechGroupComm
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechGrp2Grp(tech, region, group, groupp, year, slice)$meqTechGrp2Grp(tech, region, group, groupp, year, slice)
print("eqTechGrp2Grp(tech, region, group, groupp, year, slice)...")
@constraint(
    model,
    [(t, r, g, gp, y, s) in meqTechGrp2Grp],
    (
        if haskey(pTechGinp2use, (t, g, r, y, s))
            pTechGinp2use[(t, g, r, y, s)]
        else
            pTechGinp2useDef
        end
    ) * sum(
        (
            if (t, c, r, y, s) in mvTechInp
                (
                    vTechInp[(t, c, r, y, s)] * (
                        if haskey(pTechCinp2ginp, (t, c, r, y, s))
                            pTechCinp2ginp[(t, c, r, y, s)]
                        else
                            pTechCinp2ginpDef
                        end
                    )
                )
            else
                0
            end
        ) for c in comm if (t, g, c) in mTechGroupComm
    ) == sum(
        (
            if (t, cp, r, y, s) in mvTechOut
                (
                    (vTechOut[(t, cp, r, y, s)]) / (
                        (
                            if haskey(pTechUse2cact, (t, cp, r, y, s))
                                pTechUse2cact[(t, cp, r, y, s)]
                            else
                                pTechUse2cactDef
                            end
                        ) * (
                            if haskey(pTechCact2cout, (t, cp, r, y, s))
                                pTechCact2cout[(t, cp, r, y, s)]
                            else
                                pTechCact2coutDef
                            end
                        )
                    )
                )
            else
                0
            end
        ) for cp in comm if (t, gp, cp) in mTechGroupComm
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechShareInpLo(tech, region, group, comm, year, slice)$meqTechShareInpLo(tech, region, group, comm, year, slice)
print("eqTechShareInpLo(tech, region, group, comm, year, slice)...")
@constraint(
    model,
    [(t, r, g, c, y, s) in meqTechShareInpLo],
    vTechInp[(t, c, r, y, s)] >=
    (
        if haskey(pTechShareLo, (t, c, r, y, s))
            pTechShareLo[(t, c, r, y, s)]
        else
            pTechShareLoDef
        end
    ) * sum(
        (
            if (t, cp, r, y, s) in mvTechInp
                vTechInp[(t, cp, r, y, s)]
            else
                0
            end
        ) for cp in comm if (t, g, cp) in mTechGroupComm
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechShareInpUp(tech, region, group, comm, year, slice)$meqTechShareInpUp(tech, region, group, comm, year, slice)
print("eqTechShareInpUp(tech, region, group, comm, year, slice)...")
@constraint(
    model,
    [(t, r, g, c, y, s) in meqTechShareInpUp],
    vTechInp[(t, c, r, y, s)] <=
    (
        if haskey(pTechShareUp, (t, c, r, y, s))
            pTechShareUp[(t, c, r, y, s)]
        else
            pTechShareUpDef
        end
    ) * sum(
        (
            if (t, cp, r, y, s) in mvTechInp
                vTechInp[(t, cp, r, y, s)]
            else
                0
            end
        ) for cp in comm if (t, g, cp) in mTechGroupComm
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechShareOutLo(tech, region, group, comm, year, slice)$meqTechShareOutLo(tech, region, group, comm, year, slice)
print("eqTechShareOutLo(tech, region, group, comm, year, slice)...")
@constraint(
    model,
    [(t, r, g, c, y, s) in meqTechShareOutLo],
    vTechOut[(t, c, r, y, s)] >=
    (
        if haskey(pTechShareLo, (t, c, r, y, s))
            pTechShareLo[(t, c, r, y, s)]
        else
            pTechShareLoDef
        end
    ) * sum(
        (
            if (t, cp, r, y, s) in mvTechOut
                vTechOut[(t, cp, r, y, s)]
            else
                0
            end
        ) for cp in comm if (t, g, cp) in mTechGroupComm
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechShareOutUp(tech, region, group, comm, year, slice)$meqTechShareOutUp(tech, region, group, comm, year, slice)
print("eqTechShareOutUp(tech, region, group, comm, year, slice)...")
@constraint(
    model,
    [(t, r, g, c, y, s) in meqTechShareOutUp],
    vTechOut[(t, c, r, y, s)] <=
    (
        if haskey(pTechShareUp, (t, c, r, y, s))
            pTechShareUp[(t, c, r, y, s)]
        else
            pTechShareUpDef
        end
    ) * sum(
        (
            if (t, cp, r, y, s) in mvTechOut
                vTechOut[(t, cp, r, y, s)]
            else
                0
            end
        ) for cp in comm if (t, g, cp) in mTechGroupComm
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
# eqTechAInp(tech, comm, region, year, slice)$mvTechAInp(tech, comm, region, year, slice)
print("eqTechAInp(tech, comm, region, year, slice)...")
@constraint(
    model,
    [(t, c, r, y, s) in mvTechAInp],
    vTechAInp[(t, c, r, y, s)] ==
    (
        if (t, c, r, y, s) in mTechAct2AInp
            (vTechAct[(t, r, y, s)] * (
                if haskey(pTechAct2AInp, (t, c, r, y, s))
                    pTechAct2AInp[(t, c, r, y, s)]
                else
                    pTechAct2AInpDef
                end
            ))
        else
            0
        end
    ) +
    (
        if (t, c, r, y, s) in mTechCap2AInp
            (
                (vTechCap[(t, r, y)] * (
                    if haskey(pTechCap2AInp, (t, c, r, y, s))
                        pTechCap2AInp[(t, c, r, y, s)]
                    else
                        pTechCap2AInpDef
                    end
                )) / ((
                    if haskey(pTechCap2act, (t))
                        pTechCap2act[(t)]
                    else
                        pTechCap2actDef
                    end
                ))
            )
        else
            0
        end
    ) +
    (
        if (t, c, r, y, s) in mTechNCap2AInp
            (vTechNewCap[(t, r, y)] * (
                if haskey(pTechNCap2AInp, (t, c, r, y, s))
                    pTechNCap2AInp[(t, c, r, y, s)]
                else
                    pTechNCap2AInpDef
                end
            ))
        else
            0
        end
    ) +
    sum(
        (
            if haskey(pTechCinp2AInp, (t, c, cp, r, y, s))
                pTechCinp2AInp[(t, c, cp, r, y, s)]
            else
                pTechCinp2AInpDef
            end
        ) * vTechInp[(t, cp, r, y, s)] for
        cp in comm if (t, c, cp, r, y, s) in mTechCinp2AInp
    ) +
    sum(
        (
            if haskey(pTechCout2AInp, (t, c, cp, r, y, s))
                pTechCout2AInp[(t, c, cp, r, y, s)]
            else
                pTechCout2AInpDef
            end
        ) * vTechOut[(t, cp, r, y, s)] for
        cp in comm if (t, c, cp, r, y, s) in mTechCout2AInp
    )
);
print(
    " ",
    Dates.format(now(), "HH:MM:SS"),
    "
",
)
