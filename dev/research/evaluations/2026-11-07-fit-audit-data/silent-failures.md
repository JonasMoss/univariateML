# Silent-failure reproductions

Every INDEX can be replayed with `/tmp/umvl-cpu/run python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py INDEX` after building the harness. Inputs are exact binary64 literals, with at most twelve observations in these stress cases. The generated route calls the real C++ fitter through the shared binding controls. Reference values below are rounded to 28 significant digits; the full working-precision values are retained in `reference.jsonl.gz`. A better point is a numerical counterexample, not a general global certificate.

## Case 177: betapr, lower-1e-06

Data: `[1e-06, 2e-06, 3e-06, 4.9999999999999996e-06, 9e-06]`. Holds: `{}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 177` (run through the slot wrapper).
Returned: `{'shape1': '2.617252391165557', 'shape2': '8.1029664282539962', 'scale': '1.0911913429901832e-05'}`, status `0`, logLik `57.749475742325188321046880446374416351318359375`.
Independent logLik at that point: `57.74947574232517779954622219` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.48e-08`.
Better feasible point: `[2.617252337322758117631298081, 8.102966510861948596509650751, 0.00001091191380950434610947180392]`; logLik `57.74947574232517806457620776`.
Stationary local point: `[2.617252337322758117631298081, 8.102966510861948596509650751, 0.00001091191380950434610947180392]`; logLik `57.74947574232517806457620776`. Verified coefficient relative error: `3.48e-08`.
Local information eigenvalue range: `[0.0007530214756386312889860497561, 6.209712044326035104637834353]`; per-observation stationarity residual `6.349097960203352087233055825e-53`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 179: betapr, lower-1e-17

Data: `[1e-17, 2e-17, 3e-17, 5.0000000000000005e-17, 9.000000000000001e-17]`. Holds: `{}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 179` (run through the slot wrapper).
Returned: `{'shape1': '2.6172522886088294', 'shape2': '8.1029670767015265', 'scale': '1.0911914938991694e-16'}`, status `0`, logLik `184.391655856997687124021467752754688262939453125`.
Independent logLik at that point: `184.3916558569976900158607041` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.04e-07`.
Better feasible point: `[2.617252337322757859991741635, 8.102966510861951588355376699, 1.091191380950435275338569827e-16]`; logLik `184.3916558569976902307417694`.
Stationary local point: `[2.617252337322757859991741635, 8.102966510861951588355376699, 1.091191380950435275338569827e-16]`; logLik `184.3916558569976902307417694`. Verified coefficient relative error: `1.04e-07`.
Local information eigenvalue range: `[0.000753021648208350873556981335, 6.209712690890256676513414689]`; per-observation stationarity residual `1.593143958196741322760359377e-48`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 180: betapr, lower-1e-200

Data: `[1e-200, 2e-200, 3e-200, 5e-200, 9e-200]`. Holds: `{}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 180` (run through the slot wrapper).
Returned: `{'shape1': '2.6172520356210245', 'shape2': '8.1029698673025372', 'scale': '1.0911920061288937e-199'}`, status `0`, logLik `2291.2570159465494725736789405345916748046875`.
Independent logLik at that point: `2291.257015946549490041740731` (280 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.73e-07`.
Better feasible point: `[2.617252337322758416277218481, 8.102966510861945022479660006, 1.091191380950433969831535431e-199]`; logLik `2291.257015946549491419881325`.
Stationary local point: `[2.617252337322758416277218481, 8.102966510861945022479660006, 1.091191380950433969831535431e-199]`; logLik `2291.257015946549491419881325`. Verified coefficient relative error: `5.73e-07`.
Local information eigenvalue range: `[0.0007530222705827095487466383891, 6.209715360297773930665697785]`; per-observation stationarity residual `7.903287319863944337452459667e-45`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 185: betapr, interior

Data: `[0.18666916871781813, 0.27261096086292663, 0.1484646969214721, 0.3988768449995832, 0.439429737837367, 1.901375652366606, 0.850681333541539, 0.10314927998288996, 1.7856179604413502, 1.5002084830804323, 1.3375151204526194, 0.6510020230586888]`. Holds: `{'shape1': 2.014271442794272}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 185` (run through the slot wrapper).
Returned: `{'shape1': '2.0142714427942718', 'shape2': '4.4595811229631206', 'scale': '1.4062822299042848'}`, status `0`, logLik `-9.212680493010612536863845889456570148468017578125`.
Independent logLik at that point: `-9.212680493010623777133474952` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.03e-07`.
Better feasible point: `[2.014271442794271838039321665, 4.459580742605025015298178582, 1.406282085103007333640856409]`; logLik `-9.212680493010621916429587813`.
Stationary local point: `[2.014271442794271838039321665, 4.459580742605025015298178582, 1.406282085103007333640856409]`; logLik `-9.212680493010621916429587813`. Verified coefficient relative error: `1.03e-07`.
Local information eigenvalue range: `[0.01727433701732400272807793181, 2.836452547670611193850392665]`; per-observation stationarity residual `2.353807275455809721635154215e-55`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 196: betapr, lower-1e-06

Data: `[1e-06, 2e-06, 3e-06, 4.9999999999999996e-06, 9e-06]`. Holds: `{'shape1': 2.014271442794272}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 196` (run through the slot wrapper).
Returned: `{'shape1': '2.0142714427942718', 'shape2': '259.17436272503215', 'scale': '0.00051269254539010778'}`, status `0`, logLik `57.744766387011139840979012660682201385498046875`.
Independent logLik at that point: `57.74476638701112452848162942` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.39e-06`.
Better feasible point: `[2.014271442794271838039321665, 259.1727141267989841406027652, 0.0005126892716137139107859038624]`; logLik `57.74476638701112730357285531`.
Stationary local point: `[2.014271442794271838039321665, 259.1727141267989841406027652, 0.0005126892716137139107859038624]`; logLik `57.74476638701112730357285531`. Verified coefficient relative error: `6.39e-06`.
Local information eigenvalue range: `[0.00001366448548001937817686035789, 3.997568843754631131413197775]`; per-observation stationarity residual `3.007366045536853918676193658e-43`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 197: betapr, lower-1e-12

Data: `[1e-12, 2e-12, 3e-12, 5e-12, 9e-12]`. Holds: `{'shape1': 2.014271442794272}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 197` (run through the slot wrapper).
Returned: `{'shape1': '2.0142714427942718', 'shape2': '259.17130623240502', 'scale': '5.1268647582555358e-10'}`, status `0`, logLik `126.822319176832507992003229446709156036376953125`.
Independent logLik at that point: `126.8223191768324959979794717` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.45e-06`.
Better feasible point: `[2.014271442794271838039321665, 259.1727141268035165301030461, 5.126892716137229027271444254e-10]`; logLik `126.8223191768324980219210463`.
Stationary local point: `[2.014271442794271838039321665, 259.1727141268035165301030461, 5.126892716137229027271444254e-10]`; logLik `126.8223191768324980219210463`. Verified coefficient relative error: `5.45e-06`.
Local information eigenvalue range: `[0.00001366416256321846955305240882, 3.997474375279639480675912412]`; per-observation stationarity residual `8.50689370682696891004411846e-44`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 198: betapr, lower-1e-17

Data: `[1e-17, 2e-17, 3e-17, 5.0000000000000005e-17, 9.000000000000001e-17]`. Holds: `{'shape1': 2.014271442794272}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 198` (run through the slot wrapper).
Returned: `{'shape1': '2.0142714427942718', 'shape2': '259.17557309316163', 'scale': '5.126949489318318e-15'}`, status `0`, logLik `184.386946501683638643953599967062473297119140625`.
Independent logLik at that point: `184.3869465016836311328404211` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.11e-05`.
Better feasible point: `[2.014271442794271838039321665, 259.1727141268002576996418727, 5.126892716137164873904964814e-15]`; logLik `184.3869465016836394785190251`.
Stationary local point: `[2.014271442794271838039321665, 259.1727141268002576996418727, 5.126892716137164873904964814e-15]`; logLik `184.3869465016836394785190251`. Verified coefficient relative error: `1.11e-05`.
Local information eigenvalue range: `[0.00001366461335581532479747491039, 3.997606253488121900474790309]`; per-observation stationarity residual `3.13167711532350281456933174e-70`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 199: betapr, lower-1e-200

Data: `[1e-200, 2e-200, 3e-200, 5e-200, 9e-200]`. Holds: `{'shape1': 2.014271442794272}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 199` (run through the slot wrapper).
Returned: `{'shape1': '2.0142714427942718', 'shape2': '259.16440245598369', 'scale': '5.126727663481572e-198'}`, status `0`, logLik `2291.25230659123553778044879436492919921875`.
Independent logLik at that point: `2291.252306591235370113834085` (280 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.22e-05`.
Better feasible point: `[2.014271442794271838039321665, 259.1727141267985020890367259, 5.126892716137129661283976155e-198]`; logLik `2291.252306591235440657420297`.
Stationary local point: `[2.014271442794271838039321665, 259.1727141267985020890367259, 5.126892716137129661283976155e-198]`; logLik `2291.252306591235440657420297`. Verified coefficient relative error: `3.22e-05`.
Local information eigenvalue range: `[0.00001366343319700168248639405932, 3.997261001078704061455674108]`; per-observation stationarity residual `9.55867236955040199298726186e-70`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 200: betapr, scale-1e-150

Data: `[1.8666916871781815e-151, 2.7261096086292664e-151, 1.484646969214721e-151, 3.988768449995832e-151, 4.39429737837367e-151, 1.901375652366606e-150, 8.50681333541539e-151, 1.0314927998288996e-151, 1.7856179604413502e-150, 1.5002084830804323e-150, 1.3375151204526194e-150, 6.510020230586889e-151]`. Holds: `{'shape1': 2.014271442794272}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 200` (run through the slot wrapper).
Returned: `{'shape1': '2.0142714427942718', 'shape2': '4.4595810228821513', 'scale': '1.4062821918037193e-150'}`, status `0`, logLik `4135.4404868962728869519196450710296630859375`.
Independent logLik at that point: `4135.440486896271608245412481` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `7.59e-08`.
Better feasible point: `[2.014271442794271838039321665, 4.459580742605025436298768733, 1.406282085103007508879176373e-150]`; logLik `4135.440486896271609255751509`.
Stationary local point: `[2.014271442794271838039321665, 4.459580742605025436298768733, 1.406282085103007508879176373e-150]`; logLik `4135.440486896271609255751509`. Verified coefficient relative error: `7.59e-08`.
Local information eigenvalue range: `[0.0172743361474894099349723209, 2.836452409490981329221928365]`; per-observation stationarity residual `2.046112287614758594184239954e-56`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 201: betapr, scale-1e+150

Data: `[1.8666916871781812e+149, 2.726109608629266e+149, 1.484646969214721e+149, 3.988768449995832e+149, 4.39429737837367e+149, 1.9013756523666062e+150, 8.50681333541539e+149, 1.0314927998288997e+149, 1.78561796044135e+150, 1.5002084830804324e+150, 1.3375151204526195e+150, 6.510020230586888e+149]`. Holds: `{'shape1': 2.014271442794272}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 201` (run through the slot wrapper).
Returned: `{'shape1': '2.0142714427942718', 'shape2': '4.4595810228824266', 'scale': '1.4062821918038458e+150'}`, status `0`, logLik `-4153.8658478822935649077408015727996826171875`.
Independent logLik at that point: `-4153.865847882292854217032873` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `7.59e-08`.
Better feasible point: `[2.014271442794271838039321665, 4.459580742605024303111807715, 1.406282085103007059254980302e+150]`; logLik `-4153.86584788229285320669144`.
Stationary local point: `[2.014271442794271838039321665, 4.459580742605024303111807715, 1.406282085103007059254980302e+150]`; logLik `-4153.86584788229285320669144`. Verified coefficient relative error: `7.59e-08`.
Local information eigenvalue range: `[0.01727433614749213400454143429, 2.83645240949139899379220008]`; per-observation stationarity residual `2.046136361607132357056229563e-56`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 202: betapr, limit-outlier

Data: `[0.18666916871781813, 0.27261096086292663, 0.1484646969214721, 0.3988768449995832, 1000001.9013756524]`. Holds: `{'shape1': 2.014271442794272}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 202` (run through the slot wrapper).
Returned: `{'shape1': '2.0142714427942718', 'shape2': '0.23233207121378538', 'scale': '0.033044928255643827'}`, status `0`, logLik `-21.33015352993763968925122753717005252838134765625`.
Independent logLik at that point: `-21.33015352993764109107076081` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.53e-08`.
Better feasible point: `[2.014271442794271838039321665, 0.2323320695655663258916297354, 0.0330449270899687449102612846]`; logLik `-21.3301535299376406010985764`.
Stationary local point: `[2.014271442794271838039321665, 0.2323320695655663258916297354, 0.0330449270899687449102612846]`; logLik `-21.3301535299376406010985764`. Verified coefficient relative error: `3.53e-08`.
Local information eigenvalue range: `[0.1503864736980172114624375859, 1.084788145421293199847473924]`; per-observation stationarity residual `7.589288391508541254531821427e-61`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 204: betapr, interior

Data: `[0.18666916871781813, 0.27261096086292663, 0.1484646969214721, 0.3988768449995832, 0.439429737837367, 1.901375652366606, 0.850681333541539, 0.10314927998288996, 1.7856179604413502, 1.5002084830804323, 1.3375151204526194, 0.6510020230586888]`. Holds: `{'shape2': 3.2600098684669976}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 204` (run through the slot wrapper).
Returned: `{'shape1': '1.9450981021461342', 'shape2': '3.2600098684669976', 'scale': '1.0098664572952574'}`, status `0`, logLik `-9.2414061720655009679603608674369752407073974609375`.
Independent logLik at that point: `-9.241406172065508321019708219` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.38e-08`.
Better feasible point: `[1.945098121119016194853118687, 3.260009868466997584590671977, 1.009866443340917917020732762]`; logLik `-9.241406172065508144141390634`.
Stationary local point: `[1.945098121119016194853118687, 3.260009868466997584590671977, 1.009866443340917917020732762]`; logLik `-9.241406172065508144141390634`. Verified coefficient relative error: `1.38e-08`.
Local information eigenvalue range: `[0.1009601334181295283531091003, 2.639167678440594622084769177]`; per-observation stationarity residual `2.296580949144910565850750242e-61`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 215: betapr, lower-1e-06

Data: `[1e-06, 2e-06, 3e-06, 4.9999999999999996e-06, 9e-06]`. Holds: `{'shape2': 3.2600098684669976}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 215` (run through the slot wrapper).
Returned: `{'shape1': '4.6442575647830973', 'shape2': '3.2600098684669976', 'scale': '2.0531444518465597e-06'}`, status `0`, logLik `57.73429662859751232417693245224654674530029296875`.
Independent logLik at that point: `57.73429662859750818637450794` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `7.28e-08`.
Better feasible point: `[4.644257849835964461526052877, 3.260009868466997584590671977, 0.000002053144302428970269253955809]`; logLik `57.73429662859750917308069888`.
Stationary local point: `[4.644257849835964461526052877, 3.260009868466997584590671977, 0.000002053144302428970269253955809]`; logLik `57.73429662859750917308069888`. Verified coefficient relative error: `7.28e-08`.
Local information eigenvalue range: `[0.04319871287858210436794650412, 3.91795365305868803395523638]`; per-observation stationarity residual `9.529880904179211163408089386e-55`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 216: betapr, lower-1e-12

Data: `[1e-12, 2e-12, 3e-12, 5e-12, 9e-12]`. Holds: `{'shape2': 3.2600098684669976}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 216` (run through the slot wrapper).
Returned: `{'shape1': '4.6442575647830751', 'shape2': '3.2600098684669976', 'scale': '2.0531444518465717e-12'}`, status `0`, logLik `126.811849418418887580628506839275360107421875`.
Independent logLik at that point: `126.8118494184188788405103629` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `7.28e-08`.
Better feasible point: `[4.644257849835964885561988909, 3.260009868466997584590671977, 2.053144302428970080639994537e-12]`; logLik `126.811849418418879827216714`.
Stationary local point: `[4.644257849835964885561988909, 3.260009868466997584590671977, 2.053144302428970080639994537e-12]`; logLik `126.811849418418879827216714`. Verified coefficient relative error: `7.28e-08`.
Local information eigenvalue range: `[0.04319871287858221572973604329, 3.917953653058685885673793024]`; per-observation stationarity residual `9.52988942777978532927186326e-55`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 217: betapr, lower-1e-17

Data: `[1e-17, 2e-17, 3e-17, 5.0000000000000005e-17, 9.000000000000001e-17]`. Holds: `{'shape2': 3.2600098684669976}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 217` (run through the slot wrapper).
Returned: `{'shape1': '4.6442575647830955', 'shape2': '3.2600098684669976', 'scale': '2.0531444518465603e-17'}`, status `0`, logLik `184.3764767432700182325788773596286773681640625`.
Independent logLik at that point: `184.3764767432700203571678363` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `7.28e-08`.
Better feasible point: `[4.644257849835964678482048116, 3.260009868466997584590671977, 2.053144302428970357313756522e-17]`; logLik `184.3764767432700213438740331`.
Stationary local point: `[4.644257849835964678482048116, 3.260009868466997584590671977, 2.053144302428970357313756522e-17]`; logLik `184.3764767432700213438740331`. Verified coefficient relative error: `7.28e-08`.
Local information eigenvalue range: `[0.04319871287858209561823055076, 3.917953653058686885009352505]`; per-observation stationarity residual `9.529875797085834325550943563e-55`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 218: betapr, lower-1e-200

Data: `[1e-200, 2e-200, 3e-200, 5e-200, 9e-200]`. Holds: `{'shape2': 3.2600098684669976}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 218` (run through the slot wrapper).
Returned: `{'shape1': '4.6442575647834232', 'shape2': '3.2600098684669976', 'scale': '2.0531444518463564e-200'}`, status `0`, logLik `2291.24183683282217316445894539356231689453125`.
Independent logLik at that point: `2291.241836832821821555195193` (280 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `7.28e-08`.
Better feasible point: `[4.644257849835964473751573141, 3.260009868466997584590671977, 2.053144302428970301591402856e-200]`; logLik `2291.241836832821822541898697`.
Stationary local point: `[4.644257849835964473751573141, 3.260009868466997584590671977, 2.053144302428970301591402856e-200]`; logLik `2291.241836832821822541898697`. Verified coefficient relative error: `7.28e-08`.
Local information eigenvalue range: `[0.04319871287857975892166550095, 3.917953653058674343360277972]`; per-observation stationarity residual `9.529489414542876983450860867e-55`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 219: betapr, scale-1e-150

Data: `[1.8666916871781815e-151, 2.7261096086292664e-151, 1.484646969214721e-151, 3.988768449995832e-151, 4.39429737837367e-151, 1.901375652366606e-150, 8.50681333541539e-151, 1.0314927998288996e-151, 1.7856179604413502e-150, 1.5002084830804323e-150, 1.3375151204526194e-150, 6.510020230586889e-151]`. Holds: `{'shape2': 3.2600098684669976}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 219` (run through the slot wrapper).
Returned: `{'shape1': '1.9450988362230757', 'shape2': '3.2600098684669976', 'scale': '1.009865917390254e-150'}`, status `0`, logLik `4135.411761217217645025812089443206787109375`.
Independent logLik at that point: `4135.411761217216471736842247` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.21e-07`.
Better feasible point: `[1.945098121119016235564631972, 3.260009868466997584590671977, 1.009866443340917906420621874e-150]`; logLik `4135.411761217216723010092982`.
Stationary local point: `[1.945098121119016235564631972, 3.260009868466997584590671977, 1.009866443340917906420621874e-150]`; logLik `4135.411761217216723010092982`. Verified coefficient relative error: `5.21e-07`.
Local information eigenvalue range: `[0.1009600917356751972619508774, 2.639167938126320896900820449]`; per-observation stationarity residual `9.353348443720867483928034301e-49`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 220: betapr, scale-1e+150

Data: `[1.8666916871781812e+149, 2.726109608629266e+149, 1.484646969214721e+149, 3.988768449995832e+149, 4.39429737837367e+149, 1.9013756523666062e+150, 8.50681333541539e+149, 1.0314927998288997e+149, 1.78561796044135e+150, 1.5002084830804324e+150, 1.3375151204526195e+150, 6.510020230586888e+149]`. Holds: `{'shape2': 3.2600098684669976}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 220` (run through the slot wrapper).
Returned: `{'shape1': '1.9450988362229611', 'shape2': '3.2600098684669976', 'scale': '1.0098659173903449e+150'}`, status `0`, logLik `-4153.8945735613478973391465842723846435546875`.
Independent logLik at that point: `-4153.894573561347990678984477` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.21e-07`.
Better feasible point: `[1.945098121119016160505226484, 3.260009868466997584590671977, 1.009866443340917929179344437e+150]`; logLik `-4153.894573561347739405820541`.
Stationary local point: `[1.945098121119016160505226484, 3.260009868466997584590671977, 1.009866443340917929179344437e+150]`; logLik `-4153.894573561347739405820541`. Verified coefficient relative error: `5.21e-07`.
Local information eigenvalue range: `[0.1009600917356825464488915619, 2.639167938126292801683590961]`; per-observation stationarity residual `9.353333256257693659188608842e-49`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 221: betapr, limit-outlier

Data: `[0.18666916871781813, 0.27261096086292663, 0.1484646969214721, 0.3988768449995832, 1000001.9013756524]`. Holds: `{'shape2': 3.2600098684669976}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 221` (run through the slot wrapper).
Returned: `{'shape1': '0.07842111478219943', 'shape2': '3.2600098684669976', 'scale': '7514136.2891210057'}`, status `0`, logLik `-26.1284097337724148246707045473158359527587890625`.
Independent logLik at that point: `-26.12840973377241278811233795` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.6e-08`.
Better feasible point: `[0.07842111456761900067094627969, 3.260009868466997584590671977, 7514136.559468907016142640828]`; logLik `-26.12840973377241258298481808`.
Stationary local point: `[0.07842111456761900067094627969, 3.260009868466997584590671977, 7514136.559468907016142640828]`; logLik `-26.12840973377241258298481808`. Verified coefficient relative error: `3.6e-08`.
Local information eigenvalue range: `[0.0629976962292156488383551989, 1.013135853949373864295067298]`; per-observation stationarity residual `1.794485191995752552426696333e-59`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 247: betapr, constant-0.651002-0.0001

Data: `[0.6508718226540771, 0.6509369228563829, 0.6510020230586888, 0.6510671232609947, 0.6511973236656065]`. Holds: `{'scale': 1.0594104501122898}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 247` (run through the slot wrapper).
Returned: `{'shape1': '54549751.44848562', 'shape2': '88769957.006633952', 'scale': '1.0594104501122898'}`, status `0`, logLik `38.3904078006744384765625`.
Independent logLik at that point: `38.39040535187879776934399754` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `6.38e-08`.
Stationary local point: `[54549751.44848871704437011585, 88769957.00663899824278962848, 1.059410450112289847623969763]`; logLik `38.39040535187879776934399801`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 248: betapr, constant-0.651002-1e-08

Data: `[0.6510020100386483, 0.6510020165486686, 0.6510020230586888, 0.651002029568709, 0.6510020425887495]`. Holds: `{'scale': 1.0594104501122898}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 248` (run through the slot wrapper).
Returned: `{'shape1': '5454373797701263', 'shape2': '8876194518408743', 'scale': '1.0594104501122898'}`, status `0`, logLik `80.0`.
Independent logLik at that point: `84.44195054705917808053954505` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `0.0555`.
Stationary local point: `[5454373785714344.578068496483, 8876194498901786.640718849448, 1.059410450112289847623969763]`; logLik `84.44195054705917860975589788`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 249: betapr, constant-0.651002-1e-12

Data: `[0.6510020230573869, 0.6510020230580378, 0.6510020230586888, 0.6510020230593399, 0.6510020230606418]`. Holds: `{'scale': 1.0594104501122898}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 249` (run through the slot wrapper).
Returned: `{'shape1': '5.454590759462655e+23', 'shape2': '8.8765476096509131e+23', 'scale': '1.0594104501122898'}`, status `0`, logLik `-50465865728.0`.
Independent logLik at that point: `130.4936888567719102072437733` (127 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `1`.
Stationary local point: `[545445329477216575025049.8405, 887632390599951451696718.3318, 1.059410450112289847623969763]`; logLik `130.4936888613747955793403701`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 251: betapr, constant-1-1e-08

Data: `[0.99999998, 0.99999999, 1.0, 1.00000001, 1.00000003]`. Holds: `{'scale': 1.0594104501122898}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 251` (run through the slot wrapper).
Returned: `{'shape1': '6567301506725393', 'shape2': '6957467831348133', 'scale': '1.0594104501122898'}`, status `0`, logLik `-136.0`.
Independent logLik at that point: `82.29573789639240809556566453` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `1.61`.
Stationary local point: `[6567301472501320.283952043165, 6957467795090793.123649642467, 1.059410450112289847623969763]`; logLik `82.29573789639240814873503379`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 252: betapr, constant-1-1e-12

Data: `[0.999999999998, 0.999999999999, 1.0, 1.000000000001, 1.000000000003]`. Holds: `{'scale': 1.0594104501122898}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 252` (run through the slot wrapper).
Returned: `{'shape1': '6.5674204679584117e+23', 'shape2': '6.9575938740350953e+23', 'scale': '1.0594104501122898'}`, status `0`, logLik `-48318382080.0`.
Independent logLik at that point: `128.3474153096008516411705273` (127 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `1`.
Stationary local point: `[656723725758789089387592.9105, 695739977905399571925251.3918, 1.059410450112289847623969763]`; logLik `128.3474153236414936710667518`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 254: betapr, lower-1e-12

Data: `[1e-12, 2e-12, 3e-12, 5e-12, 9e-12]`. Holds: `{'scale': 1.0594104501122898}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 254` (run through the slot wrapper).
Returned: `{'shape1': '2.026255167714941', 'shape2': '536658974818.82312', 'scale': '1.0594104501122898'}`, status `0`, logLik `126.82125932137722656989353708922863006591796875`.
Independent logLik at that point: `126.8223009255958586464688335` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `8.21e-06`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 255: betapr, lower-1e-17

Data: `[1e-17, 2e-17, 3e-17, 5.0000000000000005e-17, 9.000000000000001e-17]`. Holds: `{'scale': 1.0594104501122898}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 255` (run through the slot wrapper).
Returned: `{'shape1': '2.0262551677082516', 'shape2': '53665897481603816', 'scale': '1.0594104501122898'}`, status `0`, logLik `-205.828955697000083091552369296550750732421875`.
Independent logLik at that point: `184.3869282504468761873621794` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `1.9`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 256: betapr, lower-1e-200

Data: `[1e-200, 2e-200, 3e-200, 5e-200, 9e-200]`. Holds: `{'scale': 1.0594104501122898}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 256` (run through the slot wrapper).
Returned: `{'shape1': '2.0262551677084244', 'shape2': '5.3665897481607234e+199', 'scale': '1.0594104501122898'}`, status `0`, logLik `-2368.01041915480664101778529584407806396484375`.
Independent logLik at that point: `2291.252288339998677363622159` (280 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `1.97`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 257: betapr, scale-1e-150

Data: `[1.8666916871781815e-151, 2.7261096086292664e-151, 1.484646969214721e-151, 3.988768449995832e-151, 4.39429737837367e-151, 1.901375652366606e-150, 8.50681333541539e-151, 1.0314927998288996e-151, 1.7856179604413502e-150, 1.5002084830804323e-150, 1.3375151204526194e-150, 6.510020230586889e-151]`. Holds: `{'scale': 1.0594104501122898}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 257` (run through the slot wrapper).
Returned: `{'shape1': '1.3768062808237056', 'shape2': '1.8278993726689006e+150', 'scale': '1.0594104501122898'}`, status `0`, logLik `-1582.05921394045662964344955980777740478515625`.
Independent logLik at that point: `4135.705570780781493388796385` (381 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `3.61`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 260: betapr, limit-concentration

Data: `[0.6510007210546427, 0.6510013720566658, 0.6510020230586888, 0.6510026740607118, 0.651003325062735]`. Holds: `{'scale': 1.0594104501122898}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 260` (run through the slot wrapper).
Returned: `{'shape1': '807247310684.30823', 'shape2': '1313676772840.9216', 'scale': '1.0594104501122898'}`, status `0`, logLik `62.4599609375`.
Independent logLik at that point: `62.39620481822837613568918799` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `0.00102`.
Stationary local point: `[807247310521.934153180656161, 1313676772576.681484955642365, 1.059410450112289847623969763]`; logLik `62.39620481822837613575025511`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 350: bhatt, interior

Data: `[0.23133425229573634, -2.4765992065164646, 2.0420503006898465, 2.850645625936443, 1.8968993409025539, -0.04324020134208423, -0.5160069093711432, -1.2007613908882648, 1.6770331044625932, -0.9195385946413882, 2.1329254007812484, -0.48213896743839824]`. Holds: `{'sigma': 1.057897350270679}`.
Call: `route_bhatt(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 350` (run through the slot wrapper).
Returned: `{'mu': '0.43067032921797926', 'sigma': '1.057897350270679', 'a': '2.0902083681589607'}`, status `0`, logLik `-22.1621814575598392593747121281921863555908203125`.
Independent logLik at that point: `-22.16218145755983743589408873` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.27e-08`.
Better feasible point: `[0.430670329022232981002271847, 1.057897350270678993311435079, 2.090208394602097648248779143]`; logLik `-22.16218145755983658874061039`.
Stationary local point: `[0.430670329022232981002271847, 1.057897350270678993311435079, 2.090208394602097648248779143]`; logLik `-22.16218145755983658874061039`. Verified coefficient relative error: `1.27e-08`.
Local information eigenvalue range: `[0.882185898551371837714418835, 12.09274075449835049420439879]`; per-observation stationarity residual `7.851279743501006571927300207e-67`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/bhatt/mle.hpp:48` (step stop): `if(std::abs(next-m)<1e-11*std::min(s,1.0))return next;`. Execution-path causation remains open.

## Case 362: bhatt, scale-1e+150

Data: `[2.3133425229573634e+149, -2.4765992065164645e+150, 2.0420503006898465e+150, 2.850645625936443e+150, 1.8968993409025537e+150, -4.324020134208423e+148, -5.160069093711432e+149, -1.2007613908882648e+150, 1.677033104462593e+150, -9.195385946413882e+149, 2.1329254007812485e+150, -4.821389674383982e+149]`. Holds: `{'sigma': 1.057897350270679}`.
Call: `route_bhatt(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 362` (run through the slot wrapper).
Returned: `{'mu': '1.599685116058775e+149', 'sigma': '1.057897350270679', 'a': '2.7124280120690724e+150'}`, status `0`, logLik `-4164.94506370622184476815164089202880859375`.
Independent logLik at that point: `-4164.945063706221928702102769` (230 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `2.88e-07`.
Better feasible point: `[1.599685116058775032882881595e+149, 1.057897350270678993311435079, 2.712156769267865504111002213e+150]`; logLik `-4164.943863646217928402078767`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/bhatt/mle.hpp:48` (step stop): `if(std::abs(next-m)<1e-11*std::min(s,1.0))return next;`. Execution-path causation remains open.

## Case 363: bhatt, offset-1e8

Data: `[100000000.23133425, 99999997.5234008, 100000002.0420503, 100000002.85064563, 100000001.89689934, 99999999.9567598, 99999999.48399308, 99999998.7992386, 100000001.67703311, 99999999.08046141, 100000002.1329254, 99999999.51786104]`. Holds: `{'sigma': 1.057897350270679}`.
Call: `route_bhatt(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 363` (run through the slot wrapper).
Returned: `{'mu': '100000000.43067034', 'sigma': '1.057897350270679', 'a': '2.0902083684174704'}`, status `0`, logLik `-22.16218146208390038509605801664292812347412109375`.
Independent logLik at that point: `-22.16218146208390137225248945` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.36e-08`.
Better feasible point: `[100000000.4306703319250651898, 1.057897350270678993311435079, 2.090208396781530303857104652]`; logLik `-22.16218146208390036152204155`.
Stationary local point: `[100000000.4306703319250651898, 1.057897350270678993311435079, 2.090208396781530303857104652]`; logLik `-22.16218146208390036152204155`. Verified coefficient relative error: `1.36e-08`.
Local information eigenvalue range: `[0.8821858995367938428795022326, 12.09274075037451798413076595]`; per-observation stationarity residual `9.456667173774732928506440809e-62`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/bhatt/mle.hpp:48` (step stop): `if(std::abs(next-m)<1e-11*std::min(s,1.0))return next;`. Execution-path causation remains open.

## Case 364: bhatt, limit-outlier

Data: `[0.23133425229573634, -2.4765992065164646, 2.0420503006898465, 2.850645625936443, 1000002.850645626]`. Holds: `{'sigma': 1.057897350270679}`.
Call: `route_bhatt(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 364` (run through the slot wrapper).
Returned: `{'mu': '500000.1870232097', 'sigma': '1.057897350270679', 'a': '500056.01887073944'}`, status `0`, logLik `-69.0781129471499610872342600487172603607177734375`.
Independent logLik at that point: `-69.07811294714996985798375254` (70 digits).
Evidence: interior dimensionless free score exceeds 1e-3 with feasible ascent. Worst relative discrepancy: `7.24e-10`.
Better feasible point: `[500000.1870232096989639103413, 1.057897350270678993311435079, 500056.0138701792521919478895]`; logLik `-69.07811289714996960798375087`.
Scaled free-coordinate score (a): `-1.0`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/bhatt/mle.hpp:48` (step stop): `if(std::abs(next-m)<1e-11*std::min(s,1.0))return next;`. Execution-path causation remains open.

## Case 370: bhatt, ties

Data: `[0.23133425229573634, 0.23133425229573634, 0.23133425229573634, 0.23133425229573634, -2.4765992065164646, 2.0420503006898465]`. Holds: `{'mu': -0.0038000682197418395, 'sigma': 1.057897350270679}`.
Call: `route_bhatt(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 370` (run through the slot wrapper).
Returned: `{'mu': '-0.0038000682197418395', 'sigma': '1.057897350270679', 'a': '1.3214506067150928'}`, status `0`, logLik `-10.222108666027072132465036702342331409454345703125`.
Independent logLik at that point: `-10.22210866602707460460741752` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.04e-08`.
Better feasible point: `[-0.003800068219741839503633684672, 1.057897350270678993311435079, 1.321450593029704025057223936]`; logLik `-10.22210866602707452053953117`.
Stationary local point: `[-0.003800068219741839503633684672, 1.057897350270678993311435079, 1.321450593029704025057223936]`; logLik `-10.22210866602707452053953117`. Verified coefficient relative error: `1.04e-08`.
Local information eigenvalue range: `[0.2612741603028731059664680367, 0.2612741603028731059664680367]`; per-observation stationarity residual `2.518651224354439912015797631e-69`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/bhatt/mle.hpp:48` (step stop): `if(std::abs(next-m)<1e-11*std::min(s,1.0))return next;`. Execution-path causation remains open.

## Case 396: bhatt, limit-outlier

Data: `[0.23133425229573634, -2.4765992065164646, 2.0420503006898465, 2.850645625936443, 1000002.850645626]`. Holds: `{'a': 1.7336058787117143}`.
Call: `route_bhatt(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 396` (run through the slot wrapper).
Returned: `{'mu': '200001.09961531952', 'sigma': '400000.87107526127', 'a': '1.7336058787117143'}`, status `0`, logLik `-71.590802740453682417864911258220672607421875`.
Independent logLik at that point: `-71.5908027404536809725235991` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.11e-08`.
Better feasible point: `[200001.0996153196738737192609, 400000.8755180836395911967856, 1.733605878711714343154426388]`; logLik `-71.59080274045368035569283083`.
Stationary local point: `[200001.0996153196738737192609, 400000.8755180836395911967856, 1.733605878711714343154426388]`; logLik `-71.59080274045368035569283083`. Verified coefficient relative error: `1.11e-08`.
Local information eigenvalue range: `[1.999999955546828972228283692, 6.25003923055685330556475448]`; per-observation stationarity residual `2.827444458274392126355554052e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/bhatt/mle.hpp:48` (step stop): `if(std::abs(next-m)<1e-11*std::min(s,1.0))return next;`. Execution-path causation remains open.

## Case 399: bhatt, tiny-2

Data: `[0.23133425229573634, -2.4765992065164646]`. Holds: `{'mu': -0.0038000682197418395, 'a': 1.7336058787117143}`.
Call: `route_bhatt(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 399` (run through the slot wrapper).
Returned: `{'mu': '-0.0038000682197418395', 'sigma': '1.39531753844177', 'a': '1.7336058787117143'}`, status `0`, logLik `-3.95040194531453181525648687966167926788330078125`.
Independent logLik at that point: `-3.950401945314531661600326627` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.42e-08`.
Better feasible point: `[-0.003800068219741839503633684672, 1.395317558212684508485976305, 1.733605878711714343154426388]`; logLik `-3.950401945314531484112902922`.
Stationary local point: `[-0.003800068219741839503633684672, 1.395317558212684508485976305, 1.733605878711714343154426388]`; logLik `-3.950401945314531484112902922`. Verified coefficient relative error: `1.42e-08`.
Local information eigenvalue range: `[0.8840160864923151699079331678, 0.8840160864923151699079331678]`; per-observation stationarity residual `3.499662294944213939499871663e-62`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/bhatt/mle.hpp:48` (step stop): `if(std::abs(next-m)<1e-11*std::min(s,1.0))return next;`. Execution-path causation remains open.

## Case 428: bhatt, limit-outlier

Data: `[0.23133425229573634, -2.4765992065164646, 2.0420503006898465, 2.850645625936443, 1000002.850645626]`. Holds: `{'sigma': 1.057897350270679, 'a': 1.7336058787117143}`.
Call: `route_bhatt(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 428` (run through the slot wrapper).
Returned: `{'mu': '199988.72526141332', 'sigma': '1.057897350270679', 'a': '1.7336058787117143'}`, status `0`, logLik `-357414216325.79608154296875`.
Independent logLik at that point: `-357414216325.7961941168713838` (70 digits).
Evidence: interior dimensionless free score exceeds 1e-3 with feasible ascent. Worst relative discrepancy: `1.12e-09`.
Better feasible point: `[200002.139774650095022798158, 1.057897350270678993311435079, 1.733605878711714343154426388]`; logLik `-357414215923.8177298548703974`.
Stationary local point: `[200002.139774650095022798158, 1.057897350270678993311435079, 1.733605878711714343154426388]`; logLik `-357414215923.8177298548703974`. Verified coefficient relative error: `—`.
Scaled free-coordinate score (mu): `11986438.82500701264064175602`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/bhatt/mle.hpp:48` (step stop): `if(std::abs(next-m)<1e-11*std::min(s,1.0))return next;`. Execution-path causation remains open.

## Case 512: bnbinom, interior

Data: `[17, 1, 2, 0, 4, 13, 1, 11, 5, 1, 0, 0]`. Holds: `{'size': 1.0672234787479615}`.
Call: `route_bnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 512` (run through the slot wrapper).
Returned: `{'size': '1.0672234787479615', 'alpha': '2.7068468805302737', 'beta': '7.9043806586553824'}`, status `0`, logLik `-31.3235757423901333140747738070785999298095703125`.
Independent logLik at that point: `-31.323575742390129143866496` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.36e-08`.
Better feasible point: `[1.067223478747961529222720856, 2.70684690996553711103158476, 7.904380766402897983689204662]`; logLik `-31.3235757423901291130089754`.
Stationary local point: `[1.067223478747961529222720856, 2.70684690996553711103158476, 7.904380766402897983689204662]`; logLik `-31.3235757423901291130089754`. Verified coefficient relative error: `1.36e-08`.
Local information eigenvalue range: `[0.01594134768784755799319100256, 1.299552132994699677341225433]`; per-observation stationarity residual `9.261913975515626292417210928e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/bnbinom/mle.hpp:34` (special difference): `ll+=d.count[i]*(increment(p[0],x)+increment(p[2],x)-increment(t,x)-special::lgamma(x+1.0));`. Execution-path causation remains open.

## Case 529: bnbinom, tiny-5

Data: `[17, 1, 2, 0, 4]`. Holds: `{'alpha': 2.141979154644717}`.
Call: `route_bnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 529` (run through the slot wrapper).
Returned: `{'size': '1.7503404470269674', 'alpha': '2.1419791546447171', 'beta': '3.4128398909353268'}`, status `0`, logLik `-13.151795594366031849631326622329652309417724609375`.
Independent logLik at that point: `-13.15179559436602974615096933` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.58e-08`.
Better feasible point: `[1.750340422866669456378896239, 2.141979154644717109334806082, 3.412839944758809196947756706]`; logLik `-13.15179559436602974159388872`.
Stationary local point: `[1.750340422866669456378896239, 2.141979154644717109334806082, 3.412839944758809196947756706]`; logLik `-13.15179559436602974159388872`. Verified coefficient relative error: `1.58e-08`.
Local information eigenvalue range: `[0.003710763786220143902298318019, 1.258060548599524406568893209]`; per-observation stationarity residual `7.299504099119470617247219638e-58`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/bnbinom/mle.hpp:34` (special difference): `ll+=d.count[i]*(increment(p[0],x)+increment(p[2],x)-increment(t,x)-special::lgamma(x+1.0));`. Execution-path causation remains open.

## Case 537: bnbinom, large-count

Data: `[1000000, 1000001, 1000003, 1000008, 1000011]`. Holds: `{'alpha': 2.141979154644717}`.
Call: `route_bnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 537` (run through the slot wrapper).
Returned: `{'size': '1463.7939229308429', 'alpha': '2.1419791546447171', 'beta': '1463.8130084261618'}`, status `0`, logLik `-71.96844304694189986548735760152339935302734375`.
Independent logLik at that point: `-71.96844304584439611674201277` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.58e-06`.
Better feasible point: `[1463.803547632647968549715327, 2.141979154644717109334806082, 1463.803547632647968549715327]`; logLik `-71.96844304584417399699245078`.
Stationary local point: `[1463.803547632647968549715327, 2.141979154644717109334806082, 1463.803547632647968549715327]`; logLik `-71.96844304584417399699245078`. Verified coefficient relative error: `6.58e-06`.
Local information eigenvalue range: `[0.0007298437491537624565148334944, 4.273634957558735466308899554]`; per-observation stationarity residual `2.775844424569506033123153673e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/bnbinom/mle.hpp:34` (special difference): `ll+=d.count[i]*(increment(p[0],x)+increment(p[2],x)-increment(t,x)-special::lgamma(x+1.0));`. Execution-path causation remains open.

## Case 555: bnbinom, tiny-2

Data: `[17, 1]`. Holds: `{'beta': 5.36473383510874}`.
Call: `route_bnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 555` (run through the slot wrapper).
Returned: `{'size': '1.6273858213864698', 'alpha': '1.6887640547005962', 'beta': '5.3647338351087397'}`, status `0`, logLik `-6.65442584695387129301025197491981089115142822265625`.
Independent logLik at that point: `-6.65442584695386934537356343` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.17e-08`.
Better feasible point: `[1.627385807154319737716982327, 1.688764034922478597223408473, 5.364733835108739690156198776]`; logLik `-6.654425846953869294991972036`.
Stationary local point: `[1.627385807154319737716982327, 1.688764034922478597223408473, 5.364733835108739690156198776]`; logLik `-6.654425846953869294991972036`. Verified coefficient relative error: `1.17e-08`.
Local information eigenvalue range: `[0.1582481585497860851825431171, 1.702919099101888320725714906]`; per-observation stationarity residual `1.493350728667898912027718685e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/bnbinom/mle.hpp:34` (special difference): `ll+=d.count[i]*(increment(p[0],x)+increment(p[2],x)-increment(t,x)-special::lgamma(x+1.0));`. Execution-path causation remains open.

## Case 636: burr, constant-2.04285-1e-12

Data: `[2.0428454965886376, 2.0428454965906804, 2.042845496592723, 2.042845496594766, 2.0428454965988516]`. Holds: `{'shape1': 1.334637717604056}`.
Call: `route_burr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 636` (run through the slot wrapper).
Returned: `{'shape1': '1.334637717604056', 'shape2': '875134658462.23279', 'scale': '2.0428454965939449'}`, status `0`, logLik `124.5463586684498267231901991181075572967529296875`.
Independent logLik at that point: `124.5463340524942894274360124` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `1.98e-07`.
Stationary local point: `[1.334637717604056028619652352, 875130348796.0063668557567458, 2.042845496593944706793805249]`; logLik `124.546334057817267377513178`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/burr/mle.hpp:351`: The held-shape path forms power*(log(x)-log(scale)); log rounding is amplified at large power. Its centered optimization uses the rounded logs at lines 310 and 344.

## Case 639: burr, constant-2.04285-1e-12

Data: `[2.0428454965886376, 2.0428454965906804, 2.042845496592723, 2.042845496594766, 2.0428454965988516]`. Holds: `{'shape1': 1.334637717604056}`.
Call: `route_burr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 639` (run through the slot wrapper).
Returned: `{'shape1': '1.334637717604056', 'shape2': '875134658462.23279', 'scale': '2.0428454965939449'}`, status `0`, logLik `124.5463586684498267231901991181075572967529296875`.
Independent logLik at that point: `124.5463340524942894274360124` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `1.98e-07`.
Stationary local point: `[1.334637717604056028619652352, 875130348796.0063668557567458, 2.042845496593944706793805249]`; logLik `124.546334057817267377513178`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/burr/mle.hpp:351`: The held-shape path forms power*(log(x)-log(scale)); log rounding is amplified at large power. Its centered optimization uses the rounded logs at lines 310 and 344.

## Case 1015: dgamma, tiny-2

Data: `[1, 0]`. Holds: `{'rate': 0.9720518551425348}`.
Call: `route_dgamma(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1015` (run through the slot wrapper).
Returned: `{'shape': '1.0919182785371475', 'rate': '0.97205185514253478'}`, status `0`, logLik `-1.914711190691235298544370380113832652568817138671875`.
Independent logLik at that point: `-1.914711190691235475041759489` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.2e-08`.
Better feasible point: `[1.091918291658392313424980965, 0.972051855142534781961671797]`; logLik `-1.914711190691235318547365873`.
Stationary local point: `[1.091918291658392313424980965, 0.972051855142534781961671797]`; logLik `-1.914711190691235318547365873`. Verified coefficient relative error: `1.2e-08`.
Local information eigenvalue range: `[1.083749668041494779419867505, 1.083749668041494779419867505]`; per-observation stationarity residual `1.390740292793574346388126567e-65`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/dgamma/mle.hpp:25` (special difference): `const double u=std::exp(shape*std::log(b)-b-special::lgamma(shape)-lp);`. Execution-path causation remains open.

## Case 1016: dgamma, tiny-3

Data: `[1, 0, 0]`. Holds: `{'rate': 0.9720518551425348}`.
Call: `route_dgamma(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1016` (run through the slot wrapper).
Returned: `{'shape': '0.80790171383585274', 'rate': '0.97205185514253478'}`, status `0`, logLik `-2.34526532474725257770842290483415126800537109375`.
Independent logLik at that point: `-2.345265324747252460973955352` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.08e-08`.
Better feasible point: `[0.8079017050891977730689029501, 0.972051855142534781961671797]`; logLik `-2.345265324747252342031713052`.
Stationary local point: `[0.8079017050891977730689029501, 0.972051855142534781961671797]`; logLik `-2.345265324747252342031713052`. Verified coefficient relative error: `1.08e-08`.
Local information eigenvalue range: `[0.6765157827530811512602387829, 0.6765157827530811512602387829]`; per-observation stationarity residual `3.211041722130202077026543469e-66`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/dgamma/mle.hpp:25` (special difference): `const double u=std::exp(shape*std::log(b)-b-special::lgamma(shape)-lp);`. Execution-path causation remains open.

## Case 1018: dgamma, ties

Data: `[1, 1, 1, 1, 0, 0]`. Holds: `{'rate': 0.9720518551425348}`.
Call: `route_dgamma(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1018` (run through the slot wrapper).
Returned: `{'shape': '1.3571311002720579', 'rate': '0.97205185514253478'}`, status `0`, logLik `-6.3877142046696899768676303210668265819549560546875`.
Independent logLik at that point: `-6.387714204669689896516075711` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.16e-08`.
Better feasible point: `[1.357131116064516426184352385, 0.972051855142534781961671797]`; logLik `-6.387714204669689287979789272`.
Stationary local point: `[1.357131116064516426184352385, 0.972051855142534781961671797]`; logLik `-6.387714204669689287979789272`. Verified coefficient relative error: `1.16e-08`.
Local information eigenvalue range: `[1.497991456724118914624439445, 1.497991456724118914624439445]`; per-observation stationarity residual `1.889883159984526064268198932e-65`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/dgamma/mle.hpp:25` (special difference): `const double u=std::exp(shape*std::log(b)-b-special::lgamma(shape)-lp);`. Execution-path causation remains open.

## Case 1283: emg, tiny-2

Data: `[2.3413394878482614, 2.2897087235884532]`. Holds: `{'mu': 0.17576157248036894}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1283` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '1.0052132339694915', 'lambda': '0.59966266417513558'}`, status `0`, logLik `-3.35709344189782665779375747661106288433074951171875`.
Independent logLik at that point: `-3.357093441897827792371953251` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.3e-08`.
Better feasible point: `[0.1757615724803689438715537108, 1.005213221335029714944503945, 0.5996626563796537789136278892]`; logLik `-3.357093441897827650712416464`.
Stationary local point: `[0.1757615724803689438715537108, 1.005213221335029714944503945, 0.5996626563796537789136278892]`; logLik `-3.357093441897827650712416464`. Verified coefficient relative error: `1.3e-08`.
Local information eigenvalue range: `[0.4330690064901116590351572248, 0.9996321665260228310238901954]`; per-observation stationarity residual `1.382491402719305230808847264e-62`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1284: emg, tiny-3

Data: `[2.3413394878482614, 2.2897087235884532, 1.3183800389288958]`. Holds: `{'mu': 0.17576157248036894}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1284` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.75661049314741291', 'lambda': '0.6602896365530132'}`, status `0`, logLik `-4.6408264278665516400224078097380697727203369140625`.
Independent logLik at that point: `-4.640826427866552273738282143` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.19e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.7566104766686418704646710914, 0.6602896220906389919013654072]`; logLik `-4.640826427866551831660767163`.
Stationary local point: `[0.1757615724803689438715537108, 0.7566104766686418704646710914, 0.6602896220906389919013654072]`; logLik `-4.640826427866551831660767163`. Verified coefficient relative error: `2.19e-08`.
Local information eigenvalue range: `[0.2153097333246549615654321795, 0.90004572231859801390288584]`; per-observation stationarity residual `1.389557962479680739585572066e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1285: emg, tiny-5

Data: `[2.3413394878482614, 2.2897087235884532, 1.3183800389288958, 0.6757171112452496, 5.611056796430911]`. Holds: `{'mu': 0.17576157248036894}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1285` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.18216277337462805', 'lambda': '0.44302720750881774'}`, status `0`, logLik `-9.089842759888394851941484375856816768646240234375`.
Independent logLik at that point: `-9.089842759888395052433237642` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.75e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.1821627629008349312281951716, 0.4430272100213868145741206921]`; logLik `-9.089842759888394705442936103`.
Stationary local point: `[0.1757615724803689438715537108, 0.1821627629008349312281951716, 0.4430272100213868145741206921]`; logLik `-9.089842759888394705442936103`. Verified coefficient relative error: `5.75e-08`.
Local information eigenvalue range: `[0.02992677157452334032722044658, 0.9936628411149539072909788215]`; per-observation stationarity residual `1.424829769552610338866985232e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1286: emg, ties

Data: `[2.3413394878482614, 2.3413394878482614, 2.3413394878482614, 2.3413394878482614, 2.2897087235884532, 1.3183800389288958]`. Holds: `{'mu': 0.17576157248036894}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1286` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.90385237533162199', 'lambda': '0.62632912479070257'}`, status `0`, logLik `-9.7645524692709440017779343179427087306976318359375`.
Independent logLik at that point: `-9.764552469270946705187795752` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.25e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.9038523715057169138078315202, 0.6263291326439741901593248283]`; logLik `-9.764552469270946237358180929`.
Stationary local point: `[0.1757615724803689438715537108, 0.9038523715057169138078315202, 0.6263291326439741901593248283]`; logLik `-9.764552469270946237358180929`. Verified coefficient relative error: `1.25e-08`.
Local information eigenvalue range: `[0.3444666580321339470109228126, 0.93549512469535020324278854]`; per-observation stationarity residual `1.18250125668395942522899421e-62`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1287: emg, constant-1.31838-0.0001

Data: `[1.31811636292111, 1.318248200925003, 1.3183800389288958, 1.3185118769327886, 1.3187755529405745]`. Holds: `{'mu': 0.17576157248036894}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1287` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.53688820633123047', 'lambda': '1.1231157979511279'}`, status `0`, logLik `-5.25562360636257608348387293517589569091796875`.
Independent logLik at that point: `-5.255623606362583630752288924` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.65e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.5368882058009192267976399767, 1.123115779473495094416650282]`; logLik `-5.255623606362583167063330047`.
Stationary local point: `[0.1757615724803689438715537108, 0.5368882058009192267976399767, 1.123115779473495094416650282]`; logLik `-5.255623606362583167063330047`. Verified coefficient relative error: `1.65e-08`.
Local information eigenvalue range: `[0.433354942163643303328845853, 0.9999999177634047531945158129]`; per-observation stationarity residual `1.362677588233520291684313638e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1288: emg, constant-1.31838-1e-08

Data: `[1.318380012561295, 1.3183800257450953, 1.3183800389288958, 1.3183800521126963, 1.318380078480297]`. Holds: `{'mu': 0.17576157248036894}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1288` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.5368758822970473', 'lambda': '1.1231417863322273'}`, status `0`, logLik `-5.2555081383762711766394204460084438323974609375`.
Independent logLik at that point: `-5.255508138376279955333325659` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `7.02e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.5368758445999583453377286649, 1.123141738687511216198580351]`; logLik `-5.255508138376272117181135985`.
Stationary local point: `[0.1757615724803689438715537108, 0.5368758445999583453377286649, 1.123141738687511216198580351]`; logLik `-5.255508138376272117181135985`. Verified coefficient relative error: `7.02e-08`.
Local information eigenvalue range: `[0.433355060761804346068256701, 1.000000112636586820071890047]`; per-observation stationarity residual `4.196789339244650750030197362e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1290: emg, constant-1.31838-0.0001

Data: `[1.31811636292111, 1.318248200925003, 1.3183800389288958, 1.3185118769327886, 1.3187755529405745]`. Holds: `{'mu': 0.17576157248036894}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1290` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.53688820633123047', 'lambda': '1.1231157979511279'}`, status `0`, logLik `-5.25562360636257608348387293517589569091796875`.
Independent logLik at that point: `-5.255623606362583630752288924` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.65e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.5368882058009192267976399767, 1.123115779473495094416650282]`; logLik `-5.255623606362583167063330047`.
Stationary local point: `[0.1757615724803689438715537108, 0.5368882058009192267976399767, 1.123115779473495094416650282]`; logLik `-5.255623606362583167063330047`. Verified coefficient relative error: `1.65e-08`.
Local information eigenvalue range: `[0.433354942163643303328845853, 0.9999999177634047531945158129]`; per-observation stationarity residual `1.362677588233520291684313638e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1291: emg, constant-1.31838-1e-08

Data: `[1.318380012561295, 1.3183800257450953, 1.3183800389288958, 1.3183800521126963, 1.318380078480297]`. Holds: `{'mu': 0.17576157248036894}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1291` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.5368758822970473', 'lambda': '1.1231417863322273'}`, status `0`, logLik `-5.2555081383762711766394204460084438323974609375`.
Independent logLik at that point: `-5.255508138376279955333325659` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `7.02e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.5368758445999583453377286649, 1.123141738687511216198580351]`; logLik `-5.255508138376272117181135985`.
Stationary local point: `[0.1757615724803689438715537108, 0.5368758445999583453377286649, 1.123141738687511216198580351]`; logLik `-5.255508138376272117181135985`. Verified coefficient relative error: `7.02e-08`.
Local information eigenvalue range: `[0.433355060761804346068256701, 1.000000112636586820071890047]`; per-observation stationarity residual `4.196789339244650750030197362e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1295: emg, offset-1e8

Data: `[100000002.34133948, 100000002.28970872, 100000001.31838004, 100000000.67571712, 100000005.61105679, 100000001.18609095, 100000002.19428764, 99999999.61377795, 100000000.59227178, 100000004.9334104, 100000000.3827321, 99999999.11081235]`. Holds: `{'mu': 0.17576157248036894}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1295` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '46986451.081956536', 'lambda': '1.2833225008157042e-08'}`, status `0`, logLik `-232.061518265766295598950819112360477447509765625`.
Independent logLik at that point: `-232.0615182657663064861484723` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.85e-08`.
Better feasible point: `[0.1757615724803689438715537108, 46986449.74168170899862774518, 0.00000001283322474624875975151411018]`; logLik `-232.0615182657663031755688628`.
Stationary local point: `[0.1757615724803689438715537108, 46986449.74168170899862774518, 0.00000001283322474624875975151411018]`; logLik `-232.0615182657663031755688628`. Verified coefficient relative error: `2.85e-08`.
Local information eigenvalue range: `[0.4333550331556749723908978179, 1.000000048933319467450378762]`; per-observation stationarity residual `2.457668107817570875147501869e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1297: emg, limit-concentration

Data: `[1.318377402168818, 1.3183787205488569, 1.3183800389288958, 1.3183813573089347, 1.3183826756889736]`. Holds: `{'mu': 0.17576157248036894}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1297` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.53687582755079932', 'lambda': '1.1231417214357753'}`, status `0`, logLik `-5.2555081268446741660227417014539241790771484375`.
Independent logLik at that point: `-5.255508126844693964137735322` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.94e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.5368758433592306856940587669, 1.123141741276335848487397886]`; logLik `-5.2555081268446925884446903`.
Stationary local point: `[0.1757615724803689438715537108, 0.5368758433592306856940587669, 1.123141741276335848487397886]`; logLik `-5.2555081268446925884446903`. Verified coefficient relative error: `2.94e-08`.
Local information eigenvalue range: `[0.4333549915293956102454279004, 0.999999952882804942213670218]`; per-observation stationarity residual `3.648421307359851840278303111e-62`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1298: emg, interior

Data: `[2.3413394878482614, 2.2897087235884532, 1.3183800389288958, 0.6757171112452496, 5.611056796430911, 1.1860909484474618, 2.1942876383991377, -0.38622205310315116, 0.5922717764038318, 4.933410406124059, 0.38273210056823587, -0.8891876547554337]`. Holds: `{'sigma': 1.0518147225573364}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1298` (run through the slot wrapper).
Returned: `{'mu': '0.045367114092040417', 'sigma': '1.0518147225573364', 'lambda': '0.6089769220825364'}`, status `0`, logLik `-23.62518921984121078594398568384349346160888671875`.
Independent logLik at that point: `-23.62518921984121333677191636` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.76e-08`.
Better feasible point: `[0.04536713684685557564617526033, 1.051814722557336434505259604, 0.608976938861391441716709325]`; logLik `-23.62518921984121107271900354`.
Stationary local point: `[0.04536713684685557564617526033, 1.051814722557336434505259604, 0.608976938861391441716709325]`; logLik `-23.62518921984121107271900354`. Verified coefficient relative error: `2.76e-08`.
Local information eigenvalue range: `[0.4889399393431750867480411626, 17.44393586118549976094094883]`; per-observation stationarity residual `4.166273865300736021948042193e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1310: emg, scale-1e+150

Data: `[2.3413394878482614e+150, 2.2897087235884532e+150, 1.3183800389288958e+150, 6.757171112452496e+149, 5.611056796430911e+150, 1.1860909484474617e+150, 2.1942876383991374e+150, -3.8622205310315114e+149, 5.922717764038318e+149, 4.933410406124059e+150, 3.8273210056823587e+149, -8.891876547554337e+149]`. Holds: `{'sigma': 1.0518147225573364}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1310` (run through the slot wrapper).
Returned: `{'mu': '-8.8918765475543375e+149', 'sigma': '1.0518147225573364', 'lambda': '3.8810036136840136e-151'}`, status `0`, logLik `-4168.704210265592337236739695072174072265625`.
Independent logLik at that point: `-4168.704210265592147253296597` (230 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.000166`.
Better feasible point: `[-8.89187719757878256917373479e+149, 1.051814722557336434505259604, 3.881003613684013596622835789e-151]`; logLik `-4168.011063387761868402483301`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1316: emg, tiny-3

Data: `[2.3413394878482614, 2.2897087235884532, 1.3183800389288958]`. Holds: `{'mu': 0.17576157248036894, 'sigma': 1.0518147225573364}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1316` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '1.0518147225573364', 'lambda': '0.74512114256421835'}`, status `0`, logLik `-4.704477955172738035116708488203585147857666015625`.
Independent logLik at that point: `-4.704477955172737985118108225` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.29e-08`.
Better feasible point: `[0.1757615724803689438715537108, 1.051814722557336434505259604, 0.7451211329434692597726989474]`; logLik `-4.704477955172737829303490232`.
Stationary local point: `[0.1757615724803689438715537108, 1.051814722557336434505259604, 0.7451211329434692597726989474]`; logLik `-4.704477955172737829303490232`. Verified coefficient relative error: `1.29e-08`.
Local information eigenvalue range: `[0.6230930998532201554773897408, 0.6230930998532201554773897408]`; per-observation stationarity residual `8.156811592810491333945989923e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1318: emg, ties

Data: `[2.3413394878482614, 2.3413394878482614, 2.3413394878482614, 2.3413394878482614, 2.2897087235884532, 1.3183800389288958]`. Holds: `{'mu': 0.17576157248036894, 'sigma': 1.0518147225573364}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1318` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '1.0518147225573364', 'lambda': '0.66424728585600601'}`, status `0`, logLik `-9.79999532965120323524388368241488933563232421875`.
Independent logLik at that point: `-9.799995329651205901730191589` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.61e-08`.
Better feasible point: `[0.1757615724803689438715537108, 1.051814722557336434505259604, 0.6642472965234939449483663439]`; logLik `-9.799995329651205386988051964`.
Stationary local point: `[0.1757615724803689438715537108, 1.051814722557336434505259604, 0.6642472965234939449483663439]`; logLik `-9.799995329651205386988051964`. Verified coefficient relative error: `1.61e-08`.
Local information eigenvalue range: `[0.6652788522156016498735277804, 0.6652788522156016498735277804]`; per-observation stationarity residual `3.634623876958791556882354892e-62`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1327: emg, offset-1e8

Data: `[100000002.34133948, 100000002.28970872, 100000001.31838004, 100000000.67571712, 100000005.61105679, 100000001.18609095, 100000002.19428764, 99999999.61377795, 100000000.59227178, 100000004.9334104, 100000000.3827321, 99999999.11081235]`. Holds: `{'mu': 0.17576157248036894, 'sigma': 1.0518147225573364}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1327` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '1.0518147225573364', 'lambda': '1.0000000467924598e-08'}`, status `0`, logLik `-233.04816910883283753719297237694263458251953125`.
Independent logLik at that point: `-233.0481691088328710857121538` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.19e-08`.
Better feasible point: `[0.1757615724803689438715537108, 1.051814722557336434505259604, 9.99999984882961634305680885e-9]`; logLik `-233.0481691088328480889966228`.
Stationary local point: `[0.1757615724803689438715537108, 1.051814722557336434505259604, 9.99999984882961634305680885e-9]`; logLik `-233.0481691088328480889966228`. Verified coefficient relative error: `6.19e-08`.
Local information eigenvalue range: `[1.000000123819001941523038989, 1.000000123819001941523038989]`; per-observation stationarity residual `2.158034329515429851570930151e-58`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1329: emg, limit-concentration

Data: `[1.318377402168818, 1.3183787205488569, 1.3183800389288958, 1.3183813573089347, 1.3183826756889736]`. Holds: `{'mu': 0.17576157248036894, 'sigma': 1.0518147225573364}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1329` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '1.0518147225573364', 'lambda': '1.4368196855785611'}`, status `0`, logLik `-6.148239237621879738071584142744541168212890625`.
Independent logLik at that point: `-6.148239237621881825177734433` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.78e-08`.
Better feasible point: `[0.1757615724803689438715537108, 1.051814722557336434505259604, 1.436819645628174722208797416]`; logLik `-6.148239237621881132741451967`.
Stationary local point: `[0.1757615724803689438715537108, 1.051814722557336434505259604, 1.436819645628174722208797416]`; logLik `-6.148239237621881132741451967`. Verified coefficient relative error: `2.78e-08`.
Local information eigenvalue range: `[0.3582633598706025408161773646, 0.3582633598706025408161773646]`; per-observation stationarity residual `1.474485808886422140490499778e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1333: emg, tiny-5

Data: `[2.3413394878482614, 2.2897087235884532, 1.3183800389288958, 0.6757171112452496, 5.611056796430911]`. Holds: `{'lambda': 1.209185930245896}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1333` (run through the slot wrapper).
Returned: `{'mu': '1.3064846493677897', 'sigma': '0.83012742896807612', 'lambda': '1.209185930245896'}`, status `0`, logLik `-9.5768727916778235709216460236348211765289306640625`.
Independent logLik at that point: `-9.576872791677823898806364075` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.68e-08`.
Better feasible point: `[1.306484642611456021887512034, 0.83012741505356518355968693, 1.20918593024589604567609058]`; logLik `-9.576872791677823762295694434`.
Stationary local point: `[1.306484642611456021887512034, 0.83012741505356518355968693, 1.20918593024589604567609058]`; logLik `-9.576872791677823762295694434`. Verified coefficient relative error: `1.68e-08`.
Local information eigenvalue range: `[0.1930479260354595221485296534, 20.25574483160148202707701736]`; per-observation stationarity residual `1.234770157192323448054895653e-58`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1346: emg, interior

Data: `[2.3413394878482614, 2.2897087235884532, 1.3183800389288958, 0.6757171112452496, 5.611056796430911, 1.1860909484474618, 2.1942876383991377, -0.38622205310315116, 0.5922717764038318, 4.933410406124059, 0.38273210056823587, -0.8891876547554337]`. Holds: `{'mu': 0.17576157248036894, 'lambda': 1.209185930245896}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1346` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '1.6315907071127018', 'lambda': '1.209185930245896'}`, status `0`, logLik `-25.147831741688268181178500526584684848785400390625`.
Independent logLik at that point: `-25.14783174168827141036799742` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.57e-08`.
Better feasible point: `[0.1757615724803689438715537108, 1.631590681551629206900569008, 1.20918593024589604567609058]`; logLik `-25.1478317416882704695717053`.
Stationary local point: `[0.1757615724803689438715537108, 1.631590681551629206900569008, 1.20918593024589604567609058]`; logLik `-25.1478317416882704695717053`. Verified coefficient relative error: `1.57e-08`.
Local information eigenvalue range: `[0.6388643335834257798319010014, 0.6388643335834257798319010014]`; per-observation stationarity residual `1.68297083446599035698399077e-66`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1349: emg, tiny-5

Data: `[2.3413394878482614, 2.2897087235884532, 1.3183800389288958, 0.6757171112452496, 5.611056796430911]`. Holds: `{'mu': 0.17576157248036894, 'lambda': 1.209185930245896}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1349` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '2.0046739319861002', 'lambda': '1.209185930245896'}`, status `0`, logLik `-11.1139740688857582284754244028590619564056396484375`.
Independent logLik at that point: `-11.1139740688857582850168637` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.95e-08`.
Better feasible point: `[0.1757615724803689438715537108, 2.004673971153360845321854831, 1.20918593024589604567609058]`; logLik `-11.11397406888575726287697189`.
Stationary local point: `[0.1757615724803689438715537108, 2.004673971153360845321854831, 1.20918593024589604567609058]`; logLik `-11.11397406888575726287697189`. Verified coefficient relative error: `1.95e-08`.
Local information eigenvalue range: `[1.071054118282767568751717448, 1.071054118282767568751717448]`; per-observation stationarity residual `3.648439951258170951437382767e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1351: emg, constant-1.31838-0.0001

Data: `[1.31811636292111, 1.318248200925003, 1.3183800389288958, 1.3185118769327886, 1.3187755529405745]`. Holds: `{'mu': 0.17576157248036894, 'lambda': 1.209185930245896}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1351` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.55298479345985085', 'lambda': '1.209185930245896'}`, status `0`, logLik `-5.26377384101741796484930091537535190582275390625`.
Independent logLik at that point: `-5.263773841017419266864953515` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.57e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.5529848076965448522519090738, 1.20918593024589604567609058]`; logLik `-5.263773841017417963921664897`.
Stationary local point: `[0.1757615724803689438715537108, 0.5529848076965448522519090738, 1.20918593024589604567609058]`; logLik `-5.263773841017417963921664897`. Verified coefficient relative error: `2.57e-08`.
Local information eigenvalue range: `[0.7863095975353327740392837346, 0.7863095975353327740392837346]`; per-observation stationarity residual `5.042403204254896209864168953e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1352: emg, constant-1.31838-1e-08

Data: `[1.318380012561295, 1.3183800257450953, 1.3183800389288958, 1.3183800521126963, 1.318380078480297]`. Holds: `{'mu': 0.17576157248036894, 'lambda': 1.209185930245896}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1352` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.55296692211979337', 'lambda': '1.209185930245896'}`, status `0`, logLik `-5.263653303370261937743634916841983795166015625`.
Independent logLik at that point: `-5.263653303370262330853161627` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.02e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.5529668999090462410669764049, 1.20918593024589604567609058]`; logLik `-5.263653303370259159464338224`.
Stationary local point: `[0.1757615724803689438715537108, 0.5529668999090462410669764049, 1.20918593024589604567609058]`; logLik `-5.263653303370259159464338224`. Verified coefficient relative error: `4.02e-08`.
Local information eigenvalue range: `[0.7862876612791098479056927904, 0.7862876612791098479056927904]`; per-observation stationarity residual `1.769009702975863925429302118e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1354: emg, constant-1.31838-0.0001

Data: `[1.31811636292111, 1.318248200925003, 1.3183800389288958, 1.3185118769327886, 1.3187755529405745]`. Holds: `{'mu': 0.17576157248036894, 'lambda': 1.209185930245896}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1354` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.55298479345985085', 'lambda': '1.209185930245896'}`, status `0`, logLik `-5.26377384101741796484930091537535190582275390625`.
Independent logLik at that point: `-5.263773841017419266864953515` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.57e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.5529848076965448522519090738, 1.20918593024589604567609058]`; logLik `-5.263773841017417963921664897`.
Stationary local point: `[0.1757615724803689438715537108, 0.5529848076965448522519090738, 1.20918593024589604567609058]`; logLik `-5.263773841017417963921664897`. Verified coefficient relative error: `2.57e-08`.
Local information eigenvalue range: `[0.7863095975353327740392837346, 0.7863095975353327740392837346]`; per-observation stationarity residual `5.042403204254896209864168953e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1355: emg, constant-1.31838-1e-08

Data: `[1.318380012561295, 1.3183800257450953, 1.3183800389288958, 1.3183800521126963, 1.318380078480297]`. Holds: `{'mu': 0.17576157248036894, 'lambda': 1.209185930245896}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1355` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.55296692211979337', 'lambda': '1.209185930245896'}`, status `0`, logLik `-5.263653303370261937743634916841983795166015625`.
Independent logLik at that point: `-5.263653303370262330853161627` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.02e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.5529668999090462410669764049, 1.20918593024589604567609058]`; logLik `-5.263653303370259159464338224`.
Stationary local point: `[0.1757615724803689438715537108, 0.5529668999090462410669764049, 1.20918593024589604567609058]`; logLik `-5.263653303370259159464338224`. Verified coefficient relative error: `4.02e-08`.
Local information eigenvalue range: `[0.7862876612791098479056927904, 0.7862876612791098479056927904]`; per-observation stationarity residual `1.769009702975863925429302118e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1357: emg, scale-1e-150

Data: `[2.3413394878482614e-150, 2.2897087235884533e-150, 1.3183800389288957e-150, 6.757171112452496e-151, 5.611056796430912e-150, 1.1860909484474618e-150, 2.1942876383991376e-150, -3.8622205310315115e-151, 5.922717764038318e-151, 4.933410406124059e-150, 3.8273210056823585e-151, -8.891876547554337e-151]`. Holds: `{'mu': 0.17576157248036894, 'lambda': 1.209185930245896}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1357` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.4923329188033515', 'lambda': '1.209185930245896'}`, status `0`, logLik `-14.2743681246911364723928272724151611328125`.
Independent logLik at that point: `-14.27436812469067227049372655` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.64e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.49233292686536472149326063, 1.20918593024589604567609058]`; logLik `-14.27436812469067105977378865`.
Stationary local point: `[0.1757615724803689438715537108, 0.49233292686536472149326063, 1.20918593024589604567609058]`; logLik `-14.27436812469067105977378865`. Verified coefficient relative error: `1.64e-08`.
Local information eigenvalue range: `[0.7525288832211925209470755721, 0.7525288832211925209470755721]`; per-observation stationarity residual `2.837529398780078054801996634e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1361: emg, limit-concentration

Data: `[1.318377402168818, 1.3183787205488569, 1.3183800389288958, 1.3183813573089347, 1.3183826756889736]`. Holds: `{'mu': 0.17576157248036894, 'lambda': 1.209185930245896}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1361` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.55296690541128068', 'lambda': '1.209185930245896'}`, status `0`, logLik `-5.263653291333213246616651304066181182861328125`.
Independent logLik at that point: `-5.263653291333214941716505651` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.32e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.5529668981156570637580691864, 1.20918593024589604567609058]`; logLik `-5.263653291333214599541719936`.
Stationary local point: `[0.1757615724803689438715537108, 0.5529668981156570637580691864, 1.20918593024589604567609058]`; logLik `-5.263653291333214599541719936`. Verified coefficient relative error: `1.32e-08`.
Local information eigenvalue range: `[0.7862876166283188644246185549, 0.7862876166283188644246185549]`; per-observation stationarity residual `2.397309527172664554904984761e-65`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 1514: foldnorm, tiny-3

Data: `[1.4745403669967405, 1.5064805568093913, 2.63759539912851]`. Holds: `{}`.
Call: `route_foldnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1514` (run through the slot wrapper).
Returned: `{'mu': '1.8728721076448804', 'sigma': '0.54089822106966001'}`, status `0`, logLik `-2.41324314224982483523262999369762837886810302734375`.
Independent logLik at that point: `-2.413243142249825322771070202` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.68e-08`.
Better feasible point: `[1.8728720972025517954425543, 0.540898257226454005528359421]`; logLik `-2.413243142249811358655446874`.
Stationary local point: `[1.8728720972025517954425543, 0.540898257226454005528359421]`; logLik `-2.413243142249811358655446874`. Verified coefficient relative error: `6.68e-08`.
Local information eigenvalue range: `[1.999994640655925363864002134, 11.98905722810103731153222341]`; per-observation stationarity residual `5.342308029295267679508928583e-55`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/foldnorm/mle.hpp:28` (step stop): `for (;k<maxit && b-a>tol;++k) {`. Execution-path causation remains open.

## Case 1519: foldnorm, constant-2.43494-1e-12

Data: `[2.4349403590372, 2.434940359039635, 2.43494035904207, 2.434940359044505, 2.434940359049375]`. Holds: `{}`.
Call: `route_foldnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1519` (run through the slot wrapper).
Returned: `{'mu': '2.4349403590425571', 'sigma': '4.1892502547547859e-12'}`, status `0`, logLik `123.89780400806813531744410283863544464111328125`.
Independent logLik at that point: `123.897826824303386963618196` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `1.84e-07`.
Stationary local point: `[2.434940359042556945468049889, 4.189231134361179672733106773e-12]`; logLik `123.8978268289025253680802906`. Verified coefficient relative error: `—`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/foldnorm/mle.hpp:28` (step stop): `for (;k<maxit && b-a>tol;++k) {`. Execution-path causation remains open.

## Case 1522: foldnorm, constant-2.43494-1e-12

Data: `[2.4349403590372, 2.434940359039635, 2.43494035904207, 2.434940359044505, 2.434940359049375]`. Holds: `{}`.
Call: `route_foldnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1522` (run through the slot wrapper).
Returned: `{'mu': '2.4349403590425571', 'sigma': '4.1892502547547859e-12'}`, status `0`, logLik `123.89780400806813531744410283863544464111328125`.
Independent logLik at that point: `123.897826824303386963618196` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `1.84e-07`.
Stationary local point: `[2.434940359042556945468049889, 4.189231134361179672733106773e-12]`; logLik `123.8978268289025253680802906`. Verified coefficient relative error: `—`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/foldnorm/mle.hpp:28` (step stop): `for (;k<maxit && b-a>tol;++k) {`. Execution-path causation remains open.

## Case 1523: foldnorm, lower-1e-06

Data: `[1e-06, 2e-06, 3e-06, 4.9999999999999996e-06, 9e-06]`. Holds: `{}`.
Call: `route_foldnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1523` (run through the slot wrapper).
Returned: `{'mu': '2.8139085557400193e-06', 'sigma': '4.0102267566726342e-06'}`, status `0`, logLik `57.51526141053962959404088906012475490570068359375`.
Independent logLik at that point: `57.51526141053962298963812524` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.41e-08`.
Better feasible point: `[0.000002813908403400896635424225401, 0.000004010226863566426302039968687]`; logLik `57.51526141053962364282493966`.
Stationary local point: `[0.000002813908403400896635424225401, 0.000004010226863566426302039968687]`; logLik `57.51526141053962364282493966`. Verified coefficient relative error: `5.41e-08`.
Local information eigenvalue range: `[0.06932392724269695224221522182, 1.295812919489098995272510518]`; per-observation stationarity residual `2.435901344830678143124261488e-55`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/foldnorm/mle.hpp:28` (step stop): `for (;k<maxit && b-a>tol;++k) {`. Execution-path causation remains open.

## Case 1524: foldnorm, lower-1e-12

Data: `[1e-12, 2e-12, 3e-12, 5e-12, 9e-12]`. Holds: `{}`.
Call: `route_foldnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1524` (run through the slot wrapper).
Returned: `{'mu': '2.8139083161590939e-12', 'sigma': '4.0102269247825228e-12'}`, status `0`, logLik `126.592814200361004850492463447153568267822265625`.
Independent logLik at that point: `126.5928142003609941567037849` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.1e-08`.
Better feasible point: `[2.813908403400897211537864155e-12, 4.01022686356642567342620648e-12]`; logLik `126.5928142003609943709255727`.
Stationary local point: `[2.813908403400897211537864155e-12, 4.01022686356642567342620648e-12]`; logLik `126.5928142003609943709255727`. Verified coefficient relative error: `3.1e-08`.
Local information eigenvalue range: `[0.06932391827611174113965760742, 1.295812975080134847333179939]`; per-observation stationarity residual `2.818161498676002251069778749e-57`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/foldnorm/mle.hpp:28` (step stop): `for (;k<maxit && b-a>tol;++k) {`. Execution-path causation remains open.

## Case 1525: foldnorm, lower-1e-17

Data: `[1e-17, 2e-17, 3e-17, 5.0000000000000005e-17, 9.000000000000001e-17]`. Holds: `{}`.
Call: `route_foldnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1525` (run through the slot wrapper).
Returned: `{'mu': '2.8139085557400196e-17', 'sigma': '4.0102267566726347e-17'}`, status `0`, logLik `184.157441525212135502442833967506885528564453125`.
Independent logLik at that point: `184.1574415252121351874146473` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.41e-08`.
Better feasible point: `[2.81390840340089720725424999e-17, 4.010226863566426420277336362e-17]`; logLik `184.1574415252121358406014577`.
Stationary local point: `[2.81390840340089720725424999e-17, 4.010226863566426420277336362e-17]`; logLik `184.1574415252121358406014577`. Verified coefficient relative error: `5.41e-08`.
Local information eigenvalue range: `[0.06932392724269700572425996225, 1.295812919489099167251104796]`; per-observation stationarity residual `2.435901303134351573395419928e-55`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/foldnorm/mle.hpp:28` (step stop): `for (;k<maxit && b-a>tol;++k) {`. Execution-path causation remains open.

## Case 1526: foldnorm, lower-1e-200

Data: `[1e-200, 2e-200, 3e-200, 5e-200, 9e-200]`. Holds: `{}`.
Call: `route_foldnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1526` (run through the slot wrapper).
Returned: `{'mu': '2.8139085557400192e-200', 'sigma': '4.0102267566726344e-200'}`, status `0`, logLik `2291.02280161476437569945119321346282958984375`.
Independent logLik at that point: `2291.022801614763936354223225` (280 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.41e-08`.
Better feasible point: `[2.813908403400896717940102157e-200, 4.010226863566426404220995288e-200]`; logLik `2291.022801614763937007410038`.
Stationary local point: `[2.813908403400896717940102157e-200, 4.010226863566426404220995288e-200]`; logLik `2291.022801614763937007410038`. Verified coefficient relative error: `5.41e-08`.
Local information eigenvalue range: `[0.06932392724269695179537644277, 1.295812919489099029828350588]`; per-observation stationarity residual `2.435901323428836322430876626e-55`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/foldnorm/mle.hpp:28` (step stop): `for (;k<maxit && b-a>tol;++k) {`. Execution-path causation remains open.

## Case 1532: foldnorm, tiny-2

Data: `[1.4745403669967405, 1.5064805568093913]`. Holds: `{'mu': 2.0248532542607856}`.
Call: `route_foldnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1532` (run through the slot wrapper).
Returned: `{'mu': '2.0248532542607856', 'sigma': '0.53458139106777947'}`, status `0`, logLik `-1.585334495113157604606612949282862246036529541015625`.
Independent logLik at that point: `-1.58533449511315765529884211` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.45e-08`.
Better feasible point: `[2.024853254260785551110757297, 0.5345813988240927618759842171]`; logLik `-1.585334495113157234269134596`.
Stationary local point: `[2.024853254260785551110757297, 0.5345813988240927618759842171]`; logLik `-1.585334495113157234269134596`. Verified coefficient relative error: `1.45e-08`.
Local information eigenvalue range: `[1.999998718916070376987548951, 1.999998718916070376987548951]`; per-observation stationarity residual `2.397485179954569470560237959e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/foldnorm/mle.hpp:28` (step stop): `for (;k<maxit && b-a>tol;++k) {`. Execution-path causation remains open.

## Case 1533: foldnorm, tiny-3

Data: `[1.4745403669967405, 1.5064805568093913, 2.63759539912851]`. Holds: `{'mu': 2.0248532542607856}`.
Call: `route_foldnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1533` (run through the slot wrapper).
Returned: `{'mu': '2.0248532542607856', 'sigma': '0.56184442195594309'}`, status `0`, logLik `-2.527224699465556501110086173866875469684600830078125`.
Independent logLik at that point: `-2.527224699465556503007958121` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.44e-08`.
Better feasible point: `[2.024853254260785551110757297, 0.5618444581565250164022089861]`; logLik `-2.527224699465544048713363015`.
Stationary local point: `[2.024853254260785551110757297, 0.5618444581565250164022089861]`; logLik `-2.527224699465544048713363015`. Verified coefficient relative error: `6.44e-08`.
Local information eigenvalue range: `[1.999994824226766304122298447, 1.999994824226766304122298447]`; per-observation stationarity residual `3.626231230280947350987226358e-55`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/foldnorm/mle.hpp:28` (step stop): `for (;k<maxit && b-a>tol;++k) {`. Execution-path causation remains open.

## Case 1535: foldnorm, ties

Data: `[1.4745403669967405, 1.4745403669967405, 1.4745403669967405, 1.4745403669967405, 1.5064805568093913, 2.63759539912851]`. Holds: `{'mu': 2.0248532542607856}`.
Call: `route_foldnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1535` (run through the slot wrapper).
Returned: `{'mu': '2.0248532542607856', 'sigma': '0.55610854532721288'}`, status `0`, logLik `-4.9928805098064277245839548413641750812530517578125`.
Independent logLik at that point: `-4.992880509806427860263393642` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.18e-08`.
Better feasible point: `[2.024853254260785551110757297, 0.556108579722428865051313921]`; logLik `-4.992880509806404907914041184`.
Stationary local point: `[2.024853254260785551110757297, 0.556108579722428865051313921]`; logLik `-4.992880509806404907914041184`. Verified coefficient relative error: `6.18e-08`.
Local information eigenvalue range: `[1.999994960662587120380800936, 1.999994960662587120380800936]`; per-observation stationarity residual `2.614369761369223592222701509e-55`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/foldnorm/mle.hpp:28` (step stop): `for (;k<maxit && b-a>tol;++k) {`. Execution-path causation remains open.

## Case 1614: fpareto, scale-1e-150

Data: `[9.341081677852826e-151, 4.595475734747349e-150, 5.998711847579505e-151, 1.056419269543656e-149, 7.000746031939794e-151, 1.112552433763363e-150, 1.3300121914598085e-148, 3.057685931487713e-151, 9.471857714817827e-151, 3.8097695246334955e-150, 1.8225442484990885e-150, 1.1865419401405472e-151]`. Holds: `{'shape1': 0.3, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1614` (run through the slot wrapper).
Returned: `{'shape1': '0.29999999999999999', 'shape2': '2.2410410158514744', 'shape3': '0.70472057380172104', 'scale': '6.0728274189573935e-151', 'min': '0'}`, status `0`, logLik `4115.497691362541445414535701274871826171875`.
Independent logLik at that point: `4115.497691362541116916041859` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.26e-08`.
Better feasible point: `[0.2999999999999999888977697537, 2.241041008225298548171448451, 0.7047205826628831535136216828, 6.072827349603119174008857047e-151, 0.0]`; logLik `4115.497691362541117007087912`.
Stationary local point: `[0.2999999999999999888977697537, 2.241041008225298548171448451, 0.7047205826628831535136216828, 6.072827349603119174008857047e-151, 0.0]`; logLik `4115.497691362541117007087912`. Verified coefficient relative error: `1.26e-08`.
Local information eigenvalue range: `[0.04829880338328586696461812632, 1.264166087334823093374839076]`; per-observation stationarity residual `2.990336218192523350947702343e-61`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1615: fpareto, scale-1e+150

Data: `[9.341081677852825e+149, 4.5954757347473486e+150, 5.998711847579505e+149, 1.056419269543656e+151, 7.000746031939793e+149, 1.1125524337633629e+150, 1.3300121914598085e+152, 3.057685931487713e+149, 9.471857714817827e+149, 3.809769524633495e+150, 1.8225442484990886e+150, 1.1865419401405473e+149]`. Holds: `{'shape1': 0.3, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1615` (run through the slot wrapper).
Returned: `{'shape1': '0.29999999999999999', 'shape2': '2.2410410158514944', 'shape3': '0.70472057380170339', 'scale': '6.0728274189575947e+149', 'min': '0'}`, status `0`, logLik `-4173.808643416023187455721199512481689453125`.
Independent logLik at that point: `-4173.808643416023345205427745` (232 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.26e-08`.
Better feasible point: `[0.2999999999999999888977697537, 2.241041008225298497727258862, 0.7047205826628832192011674997, 6.072827349603118462451105195e+149, 0.0]`; logLik `-4173.808643416023345114381327`.
Stationary local point: `[0.2999999999999999888977697537, 2.241041008225298497727258862, 0.7047205826628832192011674997, 6.072827349603118462451105195e+149, 0.0]`; logLik `-4173.808643416023345114381327`. Verified coefficient relative error: `1.26e-08`.
Local information eigenvalue range: `[0.04829880338328626317248187574, 1.26416608733483676688229234]`; per-observation stationarity residual `2.990358678078156821078710093e-61`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1633: fpareto, interior

Data: `[0.9341081677852825, 4.595475734747349, 0.5998711847579505, 10.56419269543656, 0.7000746031939793, 1.112552433763363, 133.00121914598085, 0.3057685931487713, 0.9471857714817826, 3.8097695246334955, 1.8225442484990886, 0.11865419401405472]`. Holds: `{'shape1': 0.3, 'shape2': 1.9802932530684678, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1633` (run through the slot wrapper).
Returned: `{'shape1': '0.29999999999999999', 'shape2': '1.9802932530684678', 'shape3': '0.89344039340699377', 'scale': '0.4762642465892587', 'min': '0'}`, status `0`, logLik `-29.20975803765613676432622014544904232025146484375`.
Independent logLik at that point: `-29.2097580376561843931820526` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.53e-08`.
Better feasible point: `[0.2999999999999999888977697537, 1.980293253068467818067688313, 0.8934404339170689324942913018, 0.4762642271685429778328095817, 0.0]`; logLik `-29.20975803765618276308488365`.
Stationary local point: `[0.2999999999999999888977697537, 1.980293253068467818067688313, 0.8934404339170689324942913018, 0.4762642271685429778328095817, 0.0]`; logLik `-29.20975803765618276308488365`. Verified coefficient relative error: `4.53e-08`.
Local information eigenvalue range: `[0.06818623386731833377555660546, 0.9584782343932937332026747765]`; per-observation stationarity residual `4.0391230302591319973105525e-57`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1670: fpareto, constant-1.11255-1e-12

Data: `[1.1125524337611379, 1.1125524337622505, 1.112552433763363, 1.1125524337644754, 1.1125524337667005]`. Holds: `{'shape1': 0.3, 'shape3': 0.4, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1670` (run through the slot wrapper).
Returned: `{'shape1': '0.29999999999999999', 'shape2': '2268817030881.2363', 'shape3': '0.40000000000000002', 'scale': '1.1125524337629689', 'min': '0'}`, status `0`, logLik `127.581686386114910192191018722951412200927734375`.
Independent logLik at that point: `127.5816803540562336838880786` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `4.73e-08`.
Stationary local point: `[0.2999999999999999888977697537, 2268813857203.73318485601125, 0.4000000000000000222044604925, 1.11255243376296879608155181, 0.0]`; logLik `127.581680356796037901546765`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1673: fpareto, constant-1.11255-1e-12

Data: `[1.1125524337611379, 1.1125524337622505, 1.112552433763363, 1.1125524337644754, 1.1125524337667005]`. Holds: `{'shape1': 0.3, 'shape3': 0.4, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1673` (run through the slot wrapper).
Returned: `{'shape1': '0.29999999999999999', 'shape2': '2268817030881.2363', 'shape3': '0.40000000000000002', 'scale': '1.1125524337629689', 'min': '0'}`, status `0`, logLik `127.581686386114910192191018722951412200927734375`.
Independent logLik at that point: `127.5816803540562336838880786` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `4.73e-08`.
Stationary local point: `[0.2999999999999999888977697537, 2268813857203.73318485601125, 0.4000000000000000222044604925, 1.11255243376296879608155181, 0.0]`; logLik `127.581680356796037901546765`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1681: fpareto, tiny-5

Data: `[0.9341081677852825, 4.595475734747349, 0.5998711847579505, 10.56419269543656, 0.7000746031939793]`. Holds: `{'shape2': 1.9802932530684678, 'shape3': 0.4, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1681` (run through the slot wrapper).
Returned: `{'shape1': '0.6412760494148374', 'shape2': '1.9802932530684678', 'shape3': '0.40000000000000002', 'scale': '2.6397052578410873', 'min': '0'}`, status `0`, logLik `-11.44368018019726918055312125943601131439208984375`.
Independent logLik at that point: `-11.44368018019728473648366258` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `7.55e-08`.
Better feasible point: `[0.6412760842307679174819073486, 1.980293253068467818067688313, 0.4000000000000000222044604925, 2.639705457189848064484848219, 0.0]`; logLik `-11.44368018019728424859204837`.
Stationary local point: `[0.6412760842307679174819073486, 1.980293253068467818067688313, 0.4000000000000000222044604925, 2.639705457189848064484848219, 0.0]`; logLik `-11.44368018019728424859204837`. Verified coefficient relative error: `7.55e-08`.
Local information eigenvalue range: `[0.0225563237009805952967573698, 1.053053255880184041722328721]`; per-observation stationarity residual `7.436176895640412616938472462e-56`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1682: fpareto, ties

Data: `[0.9341081677852825, 0.9341081677852825, 0.9341081677852825, 0.9341081677852825, 4.595475734747349, 0.5998711847579505]`. Holds: `{'shape2': 1.9802932530684678, 'shape3': 0.4, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1682` (run through the slot wrapper).
Returned: `{'shape1': '1.407826441417056', 'shape2': '1.9802932530684678', 'shape3': '0.40000000000000002', 'scale': '2.4929864213353685', 'min': '0'}`, status `0`, logLik `-8.601634630915309998044904205016791820526123046875`.
Independent logLik at that point: `-8.601634630915334038051696748` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.1e-08`.
Better feasible point: `[1.407826470987755520449981784, 1.980293253068467818067688313, 0.4000000000000000222044604925, 2.492986462286543120151094889, 0.0]`; logLik `-8.601634630915333929061686603`.
Stationary local point: `[1.407826470987755520449981784, 1.980293253068467818067688313, 0.4000000000000000222044604925, 2.492986462286543120151094889, 0.0]`; logLik `-8.601634630915333929061686603`. Verified coefficient relative error: `2.1e-08`.
Local information eigenvalue range: `[0.04984851744576624729328548441, 1.303887471584117820428463726]`; per-observation stationarity residual `5.271054630052569488186464887e-60`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1690: fpareto, scale-1e+150

Data: `[9.341081677852825e+149, 4.5954757347473486e+150, 5.998711847579505e+149, 1.056419269543656e+151, 7.000746031939793e+149, 1.1125524337633629e+150, 1.3300121914598085e+152, 3.057685931487713e+149, 9.471857714817827e+149, 3.809769524633495e+150, 1.8225442484990886e+150, 1.1865419401405473e+149]`. Holds: `{'shape2': 1.9802932530684678, 'shape3': 0.4, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1690` (run through the slot wrapper).
Returned: `{'shape1': '0.35007691458364376', 'shape2': '1.9802932530684678', 'shape3': '0.40000000000000002', 'scale': '1.1538941870360993e+150', 'min': '0'}`, status `0`, logLik `-4174.466341440294854692183434963226318359375`.
Independent logLik at that point: `-4174.466341440295320858433701` (232 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.49e-08`.
Better feasible point: `[0.3500769197956545109032637321, 1.980293253068467818067688313, 0.4000000000000000222044604925, 1.153894204055219403565403828e+150, 0.0]`; logLik `-4174.46634144029532025423707`.
Stationary local point: `[0.3500769197956545109032637321, 1.980293253068467818067688313, 0.4000000000000000222044604925, 1.153894204055219403565403828e+150, 0.0]`; logLik `-4174.46634144029532025423707`. Verified coefficient relative error: `1.49e-08`.
Local information eigenvalue range: `[0.164770037572906644688422463, 1.02895946688119197965323774]`; per-observation stationarity residual `1.60319317138447734142043515e-62`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1708: fpareto, interior

Data: `[0.9341081677852825, 4.595475734747349, 0.5998711847579505, 10.56419269543656, 0.7000746031939793, 1.112552433763363, 133.00121914598085, 0.3057685931487713, 0.9471857714817826, 3.8097695246334955, 1.8225442484990886, 0.11865419401405472]`. Holds: `{'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1708` (run through the slot wrapper).
Returned: `{'shape1': '0.013633129010698354', 'shape2': '50.002984512014784', 'shape3': '0.021426878081506385', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-29.0233590720517895533703267574310302734375`.
Independent logLik at that point: `-29.02335907205185524683926429` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.63e-07`.
Better feasible point: `[0.01363312542260344199121162339, 50.00299763987634549620661782, 0.02142687244108702632366977536, 1.008784292115731506811471263, 0.0]`; logLik `-29.02335907205185456272979274`.
Stationary local point: `[0.01363312542260344199121162339, 50.00299763987634549620661782, 0.02142687244108702632366977536, 1.008784292115731506811471263, 0.0]`; logLik `-29.02335907205185456272979274`. Verified coefficient relative error: `2.63e-07`.
Local information eigenvalue range: `[0.0005494055502819027256725797739, 1.564569993796482583850813595]`; per-observation stationarity residual `3.056059373398399653291164913e-47`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1711: fpareto, tiny-5

Data: `[0.9341081677852825, 4.595475734747349, 0.5998711847579505, 10.56419269543656, 0.7000746031939793]`. Holds: `{'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1711` (run through the slot wrapper).
Returned: `{'shape1': '0.026628657887690994', 'shape2': '32.502788284071407', 'shape3': '0.053408101685632391', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-10.70204503379755323067001882009208202362060546875`.
Independent logLik at that point: `-10.70204503379757835526866934` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.74e-08`.
Better feasible point: `[0.026628657428053454957756399, 32.5027888421928702681138165, 0.05340810075873715459393668911, 1.008784292115731506811471263, 0.0]`; logLik `-10.70204503379757835299422784`.
Stationary local point: `[0.026628657428053454957756399, 32.5027888421928702681138165, 0.05340810075873715459393668911, 1.008784292115731506811471263, 0.0]`; logLik `-10.70204503379757835299422784`. Verified coefficient relative error: `1.74e-08`.
Local information eigenvalue range: `[0.001013716294540279223506328943, 1.627918049871363858080972215]`; per-observation stationarity residual `5.436182294502439228650311679e-58`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1712: fpareto, ties

Data: `[0.9341081677852825, 0.9341081677852825, 0.9341081677852825, 0.9341081677852825, 4.595475734747349, 0.5998711847579505]`. Holds: `{'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1712` (run through the slot wrapper).
Returned: `{'shape1': '0.049105465874767135', 'shape2': '46.674640181697512', 'shape3': '0.06652614461023916', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-5.10922512106546644616855701315216720104217529296875`.
Independent logLik at that point: `-5.109225121065499473504192447` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.3e-07`.
Better feasible point: `[0.04910545950433692636365568617, 46.67464608690046326555527251, 0.06652613596818217277900910883, 1.008784292115731506811471263, 0.0]`; logLik `-5.109225121065498621211415335`.
Stationary local point: `[0.04910545950433692636365568617, 46.67464608690046326555527251, 0.06652613596818217277900910883, 1.008784292115731506811471263, 0.0]`; logLik `-5.109225121065498621211415335`. Verified coefficient relative error: `1.3e-07`.
Local information eigenvalue range: `[0.005704614875336457950891271088, 1.547182950775911652433393473]`; per-observation stationarity residual `7.51741843924673220676553142e-52`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1721: fpareto, limit-outlier

Data: `[0.9341081677852825, 4.595475734747349, 0.5998711847579505, 10.56419269543656, 1112685.434982509]`. Holds: `{'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1721` (run through the slot wrapper).
Returned: `{'shape1': '0.0039224098521264267', 'shape2': '60.6096912419655', 'shape3': '0.021415259227642907', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-30.2502057988159407386774546466767787933349609375`.
Independent logLik at that point: `-30.25020579881595784703812776` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.69e-08`.
Better feasible point: `[0.003922409786017799865387902789, 60.60969226285635586541143394, 0.02141525886664205434112036156, 1.008784292115731506811471263, 0.0]`; logLik `-30.25020579881595784649912026`.
Stationary local point: `[0.003922409786017799865387902789, 60.60969226285635586541143394, 0.02141525886664205434112036156, 1.008784292115731506811471263, 0.0]`; logLik `-30.25020579881595784649912026`. Verified coefficient relative error: `1.69e-08`.
Local information eigenvalue range: `[0.0002530506776100184350287723532, 1.83412839962793379320970621]`; per-observation stationarity residual `2.983581917315826017593729295e-55`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1726: fpareto, tiny-5

Data: `[0.9341081677852825, 4.595475734747349, 0.5998711847579505, 10.56419269543656, 0.7000746031939793]`. Holds: `{'shape1': 0.3, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1726` (run through the slot wrapper).
Returned: `{'shape1': '0.29999999999999999', 'shape2': '3.2724312921947405', 'shape3': '0.57405512989566465', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-10.750738420163184372313480707816779613494873046875`.
Independent logLik at that point: `-10.75073842016320639076524378` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.25e-08`.
Better feasible point: `[0.2999999999999999888977697537, 3.272431305103641031673023664, 0.5740551227199298395318377569, 1.008784292115731506811471263, 0.0]`; logLik `-10.75073842016320617082684461`.
Stationary local point: `[0.2999999999999999888977697537, 3.272431305103641031673023664, 0.5740551227199298395318377569, 1.008784292115731506811471263, 0.0]`; logLik `-10.75073842016320617082684461`. Verified coefficient relative error: `1.25e-08`.
Local information eigenvalue range: `[0.5032301935094225249046219828, 1.27148180916197038585972952]`; per-observation stationarity residual `1.88951477200903463964347133e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1738: fpareto, interior

Data: `[0.9341081677852825, 4.595475734747349, 0.5998711847579505, 10.56419269543656, 0.7000746031939793, 1.112552433763363, 133.00121914598085, 0.3057685931487713, 0.9471857714817826, 3.8097695246334955, 1.8225442484990886, 0.11865419401405472]`. Holds: `{'shape2': 1.9802932530684678, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1738` (run through the slot wrapper).
Returned: `{'shape1': '0.38738888190839921', 'shape2': '1.9802932530684678', 'shape3': '0.58438379738409429', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-29.40870837454889397122315131127834320068359375`.
Independent logLik at that point: `-29.40870837454896144496112099` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.69e-08`.
Better feasible point: `[0.3873888796333782086126426979, 1.980293253068467818067688313, 0.5843837874792372585843428944, 1.008784292115731506811471263, 0.0]`; logLik `-29.40870837454896049722582034`.
Stationary local point: `[0.3873888796333782086126426979, 1.980293253068467818067688313, 0.5843837874792372585843428944, 1.008784292115731506811471263, 0.0]`; logLik `-29.40870837454896049722582034`. Verified coefficient relative error: `1.69e-08`.
Local information eigenvalue range: `[0.4052416018028899080724775593, 1.204198940975841574575704501]`; per-observation stationarity residual `1.114345406367841942778798976e-62`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1740: fpareto, tiny-3

Data: `[0.9341081677852825, 4.595475734747349, 0.5998711847579505]`. Holds: `{'shape2': 1.9802932530684678, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1740` (run through the slot wrapper).
Returned: `{'shape1': '0.86086178755127329', 'shape2': '1.9802932530684678', 'shape3': '1.2514831889350013', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-4.8589477460568044620004002354107797145843505859375`.
Independent logLik at that point: `-4.858947746056818732997908059` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.36e-08`.
Better feasible point: `[0.8608617831016367585241433074, 1.980293253068467818067688313, 1.251483171865982669757668851, 1.008784292115731506811471263, 0.0]`; logLik `-4.858947746056818567990468253`.
Stationary local point: `[0.8608617831016367585241433074, 1.980293253068467818067688313, 1.251483171865982669757668851, 1.008784292115731506811471263, 0.0]`; logLik `-4.858947746056818567990468253`. Verified coefficient relative error: `1.36e-08`.
Local information eigenvalue range: `[0.3495327792643333987590026765, 1.656100936849062717068457653]`; per-observation stationarity residual `1.8460415226793531612729612e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1741: fpareto, tiny-5

Data: `[0.9341081677852825, 4.595475734747349, 0.5998711847579505, 10.56419269543656, 0.7000746031939793]`. Holds: `{'shape2': 1.9802932530684678, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1741` (run through the slot wrapper).
Returned: `{'shape1': '0.55671401030903911', 'shape2': '1.9802932530684678', 'shape3': '1.011618154450235', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-10.7713000856828049478508546599186956882476806640625`.
Independent logLik at that point: `-10.77130008568283812603074694` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.47e-08`.
Better feasible point: `[0.5567139742972369961546817816, 1.980293253068467818067688313, 1.011618133204689871592851426, 1.008784292115731506811471263, 0.0]`; logLik `-10.7713000856828301776864042`.
Stationary local point: `[0.5567139742972369961546817816, 1.980293253068467818067688313, 1.011618133204689871592851426, 1.008784292115731506811471263, 0.0]`; logLik `-10.7713000856828301776864042`. Verified coefficient relative error: `6.47e-08`.
Local information eigenvalue range: `[0.3662826860694849252240295002, 1.389763618887795039091419683]`; per-observation stationarity residual `6.122873484089077762830583759e-58`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1742: fpareto, ties

Data: `[0.9341081677852825, 0.9341081677852825, 0.9341081677852825, 0.9341081677852825, 4.595475734747349, 0.5998711847579505]`. Holds: `{'shape2': 1.9802932530684678, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1742` (run through the slot wrapper).
Returned: `{'shape1': '1.6269948436327144', 'shape2': '1.9802932530684678', 'shape3': '1.9295852836735015', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-6.36096792551929102188523756922222673892974853515625`.
Independent logLik at that point: `-6.360967925519317843412985059` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.79e-08`.
Better feasible point: `[1.626994814518228867289630223, 1.980293253068467818067688313, 1.929585267288111120567420968, 1.008784292115731506811471263, 0.0]`; logLik `-6.360967925519317178759626451`.
Stationary local point: `[1.626994814518228867289630223, 1.980293253068467818067688313, 1.929585267288111120567420968, 1.008784292115731506811471263, 0.0]`; logLik `-6.360967925519317178759626451`. Verified coefficient relative error: `1.79e-08`.
Local information eigenvalue range: `[0.3157507614401905005191464617, 2.353161079334756131066285086]`; per-observation stationarity residual `1.95391867110684017851591595e-62`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1751: fpareto, limit-outlier

Data: `[0.9341081677852825, 4.595475734747349, 0.5998711847579505, 10.56419269543656, 1112685.434982509]`. Holds: `{'shape2': 1.9802932530684678, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1751` (run through the slot wrapper).
Returned: `{'shape1': '0.12290460941103501', 'shape2': '1.9802932530684678', 'shape3': '0.55315761919601891', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-30.55895089541825626611171173863112926483154296875`.
Independent logLik at that point: `-30.55895089541830054323466082` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.45e-08`.
Better feasible point: `[0.1229046064030548516922082077, 1.980293253068467818067688313, 0.5531576071759277822113681766, 1.008784292115731506811471263, 0.0]`; logLik `-30.55895089541829920286605662`.
Stationary local point: `[0.1229046064030548516922082077, 1.980293253068467818067688313, 0.5531576071759277822113681766, 1.008784292115731506811471263, 0.0]`; logLik `-30.55895089541829920286605662`. Verified coefficient relative error: `2.45e-08`.
Local information eigenvalue range: `[0.2954783512122387012616986719, 1.036749029954044253245065606]`; per-observation stationarity residual `1.414860431468194838788735479e-61`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1759: fpareto, constant-1.11255-1e-08

Data: `[1.1125524115123142, 1.1125524226378387, 1.112552433763363, 1.1125524448888873, 1.112552467139936]`. Holds: `{'shape1': 0.3, 'shape2': 1.9802932530684678, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1759` (run through the slot wrapper).
Returned: `{'shape1': '0.29999999999999999', 'shape2': '1.9802932530684678', 'shape3': '0.73749649958330499', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-7.1918409956589410825245067826472222805023193359375`.
Independent logLik at that point: `-7.191840995658981822296425845` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.32e-08`.
Better feasible point: `[0.2999999999999999888977697537, 1.980293253068467818067688313, 0.7374964898379659248933152837, 1.008784292115731506811471263, 0.0]`; logLik `-7.191840995658981572849668823`.
Stationary local point: `[0.2999999999999999888977697537, 1.980293253068467818067688313, 0.7374964898379659248933152837, 1.008784292115731506811471263, 0.0]`; logLik `-7.191840995658981572849668823`. Verified coefficient relative error: `1.32e-08`.
Local information eigenvalue range: `[0.5714311053331278212897586324, 0.5714311053331278212897586324]`; per-observation stationarity residual `1.759950599079376395852465206e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1760: fpareto, constant-1.11255-1e-12

Data: `[1.1125524337611379, 1.1125524337622505, 1.112552433763363, 1.1125524337644754, 1.1125524337667005]`. Holds: `{'shape1': 0.3, 'shape2': 1.9802932530684678, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1760` (run through the slot wrapper).
Returned: `{'shape1': '0.29999999999999999', 'shape2': '1.9802932530684678', 'shape3': '0.73749649788075045', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-7.1918409889986829597319228923879563808441162109375`.
Independent logLik at that point: `-7.191840988998710150772576754` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.32e-08`.
Better feasible point: `[0.2999999999999999888977697537, 1.980293253068467818067688313, 0.7374964881354107862944082702, 1.008784292115731506811471263, 0.0]`; logLik `-7.191840988998709901325787813`.
Stationary local point: `[0.2999999999999999888977697537, 1.980293253068467818067688313, 0.7374964881354107862944082702, 1.008784292115731506811471263, 0.0]`; logLik `-7.191840988998709901325787813`. Verified coefficient relative error: `1.32e-08`.
Local information eigenvalue range: `[0.5714311058255614847882197212, 0.5714311058255614847882197212]`; per-observation stationarity residual `1.759951483482871045089490634e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1762: fpareto, constant-1.11255-1e-08

Data: `[1.1125524115123142, 1.1125524226378387, 1.112552433763363, 1.1125524448888873, 1.112552467139936]`. Holds: `{'shape1': 0.3, 'shape2': 1.9802932530684678, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1762` (run through the slot wrapper).
Returned: `{'shape1': '0.29999999999999999', 'shape2': '1.9802932530684678', 'shape3': '0.73749649958330499', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-7.1918409956589410825245067826472222805023193359375`.
Independent logLik at that point: `-7.191840995658981822296425845` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.32e-08`.
Better feasible point: `[0.2999999999999999888977697537, 1.980293253068467818067688313, 0.7374964898379659248933152837, 1.008784292115731506811471263, 0.0]`; logLik `-7.191840995658981572849668823`.
Stationary local point: `[0.2999999999999999888977697537, 1.980293253068467818067688313, 0.7374964898379659248933152837, 1.008784292115731506811471263, 0.0]`; logLik `-7.191840995658981572849668823`. Verified coefficient relative error: `1.32e-08`.
Local information eigenvalue range: `[0.5714311053331278212897586324, 0.5714311053331278212897586324]`; per-observation stationarity residual `1.759950599079376395852465206e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1763: fpareto, constant-1.11255-1e-12

Data: `[1.1125524337611379, 1.1125524337622505, 1.112552433763363, 1.1125524337644754, 1.1125524337667005]`. Holds: `{'shape1': 0.3, 'shape2': 1.9802932530684678, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1763` (run through the slot wrapper).
Returned: `{'shape1': '0.29999999999999999', 'shape2': '1.9802932530684678', 'shape3': '0.73749649788075045', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-7.1918409889986829597319228923879563808441162109375`.
Independent logLik at that point: `-7.191840988998710150772576754` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.32e-08`.
Better feasible point: `[0.2999999999999999888977697537, 1.980293253068467818067688313, 0.7374964881354107862944082702, 1.008784292115731506811471263, 0.0]`; logLik `-7.191840988998709901325787813`.
Stationary local point: `[0.2999999999999999888977697537, 1.980293253068467818067688313, 0.7374964881354107862944082702, 1.008784292115731506811471263, 0.0]`; logLik `-7.191840988998709901325787813`. Verified coefficient relative error: `1.32e-08`.
Local information eigenvalue range: `[0.5714311058255614847882197212, 0.5714311058255614847882197212]`; per-observation stationarity residual `1.759951483482871045089490634e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1800: fpareto, tiny-3

Data: `[0.9341081677852825, 4.595475734747349, 0.5998711847579505]`. Holds: `{'shape2': 1.9802932530684678, 'shape3': 0.4, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1800` (run through the slot wrapper).
Returned: `{'shape1': '0.46429911556918257', 'shape2': '1.9802932530684678', 'shape3': '0.40000000000000002', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-5.6481608713543440813964480184949934482574462890625`.
Independent logLik at that point: `-5.648160871354363441275081505` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.78e-08`.
Better feasible point: `[0.4642990933558138532989219197, 1.980293253068467818067688313, 0.4000000000000000222044604925, 1.008784292115731506811471263, 0.0]`; logLik `-5.648160871354360805809377052`.
Stationary local point: `[0.4642990933558138532989219197, 1.980293253068467818067688313, 0.4000000000000000222044604925, 1.008784292115731506811471263, 0.0]`; logLik `-5.648160871354360805809377052`. Verified coefficient relative error: `4.78e-08`.
Local information eigenvalue range: `[0.7675965026442451687475391, 0.7675965026442451687475391]`; per-observation stationarity residual `4.864351585212649605014375485e-59`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 1841: frechet, lower-1e-17

Data: `[0.061276836925042384, 0.0612768369250424, 0.061276836925042405, 0.061276836925042426, 0.06127683692504247]`. Holds: `{}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1841` (run through the slot wrapper).
Returned: `{'lambda': '3.2954512117835884', 'mu': '0.061276836925042336', 'sigma': '5.9147365013809309e-17'}`, status `0`, logLik `184.04782862054611314306384883821010589599609375`.
Independent logLik at that point: `183.8051561362532725852312628` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.00132`.
Better feasible point: `[618.3346732403560056555094373, 0.06127683692502939934313924453, 1.300360794239258462062025315e-14]`; logLik `183.8843397973781459274444801`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1847: frechet, interior

Data: `[1.049378628217082, 0.8521441150553984, 0.7225404724036851, 1.1442321023635147, 0.8365355953601462, 1.4870095975075794, 0.9416931901127262, 3.5167577928919878, 0.9383643589747765, 0.8795879557610629, 1.512374311260067, 4.029113505565651]`. Holds: `{'lambda': 1.780221601423486}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1847` (run through the slot wrapper).
Returned: `{'lambda': '1.780221601423486', 'mu': '0.50837688219472232', 'sigma': '0.43993203263453357'}`, status `0`, logLik `-8.3888505387433713877953778137452900409698486328125`.
Independent logLik at that point: `-8.388850538743367888649937124` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.68e-08`.
Better feasible point: `[1.780221601423485955351111443, 0.5083768907856599227441017464, 0.4399320208255772003800940544]`; logLik `-8.38885053874336414943866079`.
Stationary local point: `[1.780221601423485955351111443, 0.5083768907856599227441017464, 0.4399320208255772003800940544]`; logLik `-8.38885053874336414943866079`. Verified coefficient relative error: `2.68e-08`.
Local information eigenvalue range: `[0.6755685952157108642537766013, 433.094419446471411475791031]`; per-observation stationarity residual `2.857339170657134454507266191e-57`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1848: frechet, tiny-2

Data: `[1.049378628217082, 0.8521441150553984]`. Holds: `{'lambda': 1.780221601423486}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1848` (run through the slot wrapper).
Returned: `{'lambda': '1.780221601423486', 'mu': '0.79710100152705876', 'sigma': '0.078358958091607769'}`, status `0`, logLik `1.9775960014091378269540655310265719890594482421875`.
Independent logLik at that point: `1.977596001409137973980179171` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.11e-08`.
Better feasible point: `[1.780221601423485955351111443, 0.7971010021675649945968693943, 0.07835895722425159729568867144]`; logLik `1.977596001409138018863655778`.
Stationary local point: `[1.780221601423485955351111443, 0.7971010021675649945968693943, 0.07835895722425159729568867144]`; logLik `1.977596001409138018863655778`. Verified coefficient relative error: `1.11e-08`.
Local information eigenvalue range: `[0.3069799592463881445772983431, 43.93834712709968760575497348]`; per-observation stationarity residual `5.004346184634014708247672854e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1849: frechet, tiny-3

Data: `[1.049378628217082, 0.8521441150553984, 0.7225404724036851]`. Holds: `{'lambda': 1.780221601423486}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1849` (run through the slot wrapper).
Returned: `{'lambda': '1.780221601423486', 'mu': '0.6266641610813759', 'sigma': '0.15406489969975426'}`, status `0`, logLik `1.7950490585091893880331781474524177610874176025390625`.
Independent logLik at that point: `1.795049058509188323533343843` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.52e-08`.
Better feasible point: `[1.780221601423485955351111443, 0.6266641593850900753331858226, 0.154064902043934820296296106]`; logLik `1.795049058509188566715797949`.
Stationary local point: `[1.780221601423485955351111443, 0.6266641593850900753331858226, 0.154064902043934820296296106]`; logLik `1.795049058509188566715797949`. Verified coefficient relative error: `1.52e-08`.
Local information eigenvalue range: `[0.5313935575717603818580913622, 35.89551928243853928689682338]`; per-observation stationarity residual `6.859892107467774893561801562e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1852: frechet, constant-1.04938-0.0001

Data: `[1.0491687524914386, 1.0492736903542603, 1.049378628217082, 1.0494835660799038, 1.0496934418055472]`. Holds: `{'lambda': 1.780221601423486}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1852` (run through the slot wrapper).
Returned: `{'lambda': '1.780221601423486', 'mu': '1.049034009931775', 'sigma': '0.00024514712497588395'}`, status `0`, logLik `35.79468859149148585174771142192184925079345703125`.
Independent logLik at that point: `35.7946885914914738369171047` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.02e-08`.
Better feasible point: `[1.780221601423485955351111443, 1.049034009929992028445703003, 0.0002451471274656147754564200155]`; logLik `35.79468859149147408269905433`.
Stationary local point: `[1.780221601423485955351111443, 1.049034009929992028445703003, 0.0002451471274656147754564200155]`; logLik `35.79468859149147408269905433`. Verified coefficient relative error: `1.02e-08`.
Local information eigenvalue range: `[0.6865050772527965540524767123, 39.30426544438165084116429317]`; per-observation stationarity residual `1.239155369200358476067224882e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1853: frechet, constant-1.04938-1e-08

Data: `[1.0493786072295095, 1.0493786177232958, 1.049378628217082, 1.0493786387108683, 1.049378659698441]`. Holds: `{'lambda': 1.780221601423486}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1853` (run through the slot wrapper).
Returned: `{'lambda': '1.780221601423486', 'mu': '1.0493785937552536', 'sigma': '2.4514712502723744e-08'}`, status `0`, logLik `81.8463904503249750632676295936107635498046875`.
Independent logLik at that point: `81.84639045032498583206330284` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.02e-08`.
Better feasible point: `[1.780221601423485955351111443, 1.049378593755253338297300818, 0.00000002451471275169682713699878876]`; logLik `81.84639045032498622017537307`.
Stationary local point: `[1.780221601423485955351111443, 1.049378593755253338297300818, 0.00000002451471275169682713699878876]`; logLik `81.84639045032498622017537307`. Verified coefficient relative error: `1.02e-08`.
Local information eigenvalue range: `[0.6865050772527965666288699701, 39.30426544438165089063458502]`; per-observation stationarity residual `1.037279750932931966264873931e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1854: frechet, constant-1.04938-1e-12

Data: `[1.0493786282149833, 1.0493786282160327, 1.049378628217082, 1.0493786282181314, 1.0493786282202302]`. Holds: `{'lambda': 1.780221601423486}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1854` (run through the slot wrapper).
Returned: `{'lambda': '1.780221601423486', 'mu': '1.0493786282136359', 'sigma': '2.4514810022492771e-12'}`, status `0`, logLik `127.8980724063709288884638226591050624847412109375`.
Independent logLik at that point: `127.8980724063709800953135008` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.02e-08`.
Better feasible point: `[1.780221601423485955351111443, 1.049378628213635857938410911, 2.451481027146684482144285205e-12]`; logLik `127.8980724201959620635960889`.
Stationary local point: `[1.780221601423485955351111443, 1.049378628213635857938410911, 2.451481027146684482144285205e-12]`; logLik `127.8980724201959620635960889`. Verified coefficient relative error: `1.02e-08`.
Local information eigenvalue range: `[0.6865050772527965278430319538, 39.30426544438165073806725616]`; per-observation stationarity residual `1.304452730584498399220722116e-57`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1855: frechet, constant-1.04938-0.0001

Data: `[1.0491687524914386, 1.0492736903542603, 1.049378628217082, 1.0494835660799038, 1.0496934418055472]`. Holds: `{'lambda': 1.780221601423486}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1855` (run through the slot wrapper).
Returned: `{'lambda': '1.780221601423486', 'mu': '1.049034009931775', 'sigma': '0.00024514712497588395'}`, status `0`, logLik `35.79468859149148585174771142192184925079345703125`.
Independent logLik at that point: `35.7946885914914738369171047` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.02e-08`.
Better feasible point: `[1.780221601423485955351111443, 1.049034009929992028445703003, 0.0002451471274656147754564200155]`; logLik `35.79468859149147408269905433`.
Stationary local point: `[1.780221601423485955351111443, 1.049034009929992028445703003, 0.0002451471274656147754564200155]`; logLik `35.79468859149147408269905433`. Verified coefficient relative error: `1.02e-08`.
Local information eigenvalue range: `[0.6865050772527965540524767123, 39.30426544438165084116429317]`; per-observation stationarity residual `1.239155369200358476067224882e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1856: frechet, constant-1.04938-1e-08

Data: `[1.0493786072295095, 1.0493786177232958, 1.049378628217082, 1.0493786387108683, 1.049378659698441]`. Holds: `{'lambda': 1.780221601423486}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1856` (run through the slot wrapper).
Returned: `{'lambda': '1.780221601423486', 'mu': '1.0493785937552536', 'sigma': '2.4514712502723744e-08'}`, status `0`, logLik `81.8463904503249750632676295936107635498046875`.
Independent logLik at that point: `81.84639045032498583206330284` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.02e-08`.
Better feasible point: `[1.780221601423485955351111443, 1.049378593755253338297300818, 0.00000002451471275169682713699878876]`; logLik `81.84639045032498622017537307`.
Stationary local point: `[1.780221601423485955351111443, 1.049378593755253338297300818, 0.00000002451471275169682713699878876]`; logLik `81.84639045032498622017537307`. Verified coefficient relative error: `1.02e-08`.
Local information eigenvalue range: `[0.6865050772527965666288699701, 39.30426544438165089063458502]`; per-observation stationarity residual `1.037279750932931966264873931e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1857: frechet, constant-1.04938-1e-12

Data: `[1.0493786282149833, 1.0493786282160327, 1.049378628217082, 1.0493786282181314, 1.0493786282202302]`. Holds: `{'lambda': 1.780221601423486}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1857` (run through the slot wrapper).
Returned: `{'lambda': '1.780221601423486', 'mu': '1.0493786282136359', 'sigma': '2.4514810022492771e-12'}`, status `0`, logLik `127.8980724063709288884638226591050624847412109375`.
Independent logLik at that point: `127.8980724063709800953135008` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.02e-08`.
Better feasible point: `[1.780221601423485955351111443, 1.049378628213635857938410911, 2.451481027146684482144285205e-12]`; logLik `127.8980724201959620635960889`.
Stationary local point: `[1.780221601423485955351111443, 1.049378628213635857938410911, 2.451481027146684482144285205e-12]`; logLik `127.8980724201959620635960889`. Verified coefficient relative error: `1.02e-08`.
Local information eigenvalue range: `[0.6865050772527965278430319538, 39.30426544438165073806725616]`; per-observation stationarity residual `1.304452730584498399220722116e-57`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1860: frechet, lower-1e-17

Data: `[0.061276836925042384, 0.0612768369250424, 0.061276836925042405, 0.061276836925042426, 0.06127683692504247]`. Holds: `{'lambda': 1.780221601423486}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1860` (run through the slot wrapper).
Returned: `{'lambda': '1.780221601423486', 'mu': '0.06127683692504237', 'sigma': '3.0834947878143559e-17'}`, status `0`, logLik `183.692616828129075656761415302753448486328125`.
Independent logLik at that point: `183.6926168281290778239909799` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.00152`.
Better feasible point: `[1.780221601423485955351111443, 0.06127683692504236711272134643, 3.083494807020592034092740967e-17]`; logLik `183.9718301556458566881693933`.
Stationary local point: `[1.780221601423485955351111443, 0.06127683692504236711272134643, 3.083494807020592034092740967e-17]`; logLik `183.9718301556458566881693933`. Verified coefficient relative error: `—`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1861: frechet, scale-1e-150

Data: `[1.049378628217082e-150, 8.521441150553985e-151, 7.225404724036851e-151, 1.1442321023635147e-150, 8.365355953601462e-151, 1.4870095975075795e-150, 9.416931901127262e-151, 3.516757792891988e-150, 9.383643589747765e-151, 8.795879557610628e-151, 1.512374311260067e-150, 4.0291135055656513e-150]`. Holds: `{'lambda': 1.780221601423486}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1861` (run through the slot wrapper).
Returned: `{'lambda': '1.780221601423486', 'mu': '5.0837688219472226e-151', 'sigma': '4.3993203263453385e-151'}`, status `0`, logLik `4136.2643168505373978405259549617767333984375`.
Independent logLik at that point: `4136.264316850538862035165951` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.68e-08`.
Better feasible point: `[1.780221601423485955351111443, 5.083768907856598450085302531e-151, 4.399320208255772845907782049e-151]`; logLik `4136.264316850538865774377216`.
Stationary local point: `[1.780221601423485955351111443, 5.083768907856598450085302531e-151, 4.399320208255772845907782049e-151]`; logLik `4136.264316850538865774377216`. Verified coefficient relative error: `2.68e-08`.
Local information eigenvalue range: `[0.6755685952157115742820471368, 433.0944194464712951891162155]`; per-observation stationarity residual `2.857338866444766905383686784e-57`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1863: frechet, offset-1e8

Data: `[100000001.04937863, 100000000.85214412, 100000000.72254047, 100000001.14423211, 100000000.8365356, 100000001.4870096, 100000000.94169319, 100000003.51675779, 100000000.93836436, 100000000.87958795, 100000001.51237431, 100000004.0291135]`. Holds: `{'lambda': 1.780221601423486}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1863` (run through the slot wrapper).
Returned: `{'lambda': '1.780221601423486', 'mu': '100000000.50837688', 'sigma': '0.43993203756610133'}`, status `0`, logLik `-8.388850621901244863920510397292673587799072265625`.
Independent logLik at that point: `-8.388850621901240286005199421` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.81e-08`.
Better feasible point: `[1.780221601423485955351111443, 100000000.5083768835419325764, 0.4399320296149116201893239478]`; logLik `-8.388850621901237497936176767`.
Stationary local point: `[1.780221601423485955351111443, 100000000.5083768835419325764, 0.4399320296149116201893239478]`; logLik `-8.388850621901237497936176767`. Verified coefficient relative error: `1.81e-08`.
Local information eigenvalue range: `[0.6755685958986051825230138058, 433.0944050878153231291068458]`; per-observation stationarity residual `3.197233087579148362911685308e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1865: frechet, limit-concentration

Data: `[1.0493765294598256, 1.0493775788384538, 1.049378628217082, 1.0493796775957103, 1.0493807269743385]`. Holds: `{'lambda': 1.780221601423486}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1865` (run through the slot wrapper).
Returned: `{'lambda': '1.780221601423486', 'mu': '1.0493752517512593', 'sigma': '2.3377335821873791e-06'}`, status `0`, logLik `59.2447825003530539333951310254633426666259765625`.
Independent logLik at that point: `59.24478250035304388992521796` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.1e-08`.
Better feasible point: `[1.780221601423485955351111443, 1.049375251751277721562129514, 0.000002337733556521843991530803865]`; logLik `59.24478250035304418969160804`.
Stationary local point: `[1.780221601423485955351111443, 1.049375251751277721562129514, 0.000002337733556521843991530803865]`; logLik `59.24478250035304418969160804`. Verified coefficient relative error: `1.1e-08`.
Local information eigenvalue range: `[0.689207659236800942625192398, 28.75532810388636031378116608]`; per-observation stationarity residual `2.140796583715232052268465837e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1873: frechet, constant-1.04938-1e-12

Data: `[1.0493786282149833, 1.0493786282160327, 1.049378628217082, 1.0493786282181314, 1.0493786282202302]`. Holds: `{'mu': 0.06127683692504238}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1873` (run through the slot wrapper).
Returned: `{'lambda': '666338474224.99927', 'mu': '0.061276836925042377', 'sigma': '0.98810179129138798'}`, status `0`, logLik `128.280069512836462308769114315509796142578125`.
Independent logLik at that point: `128.2800695547576467323422921` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `9.06e-08`.
Better feasible point: `[666338534620.8127514365867464, 0.06127683692504237722431525981, 0.9881017912913879704147192181]`; logLik `128.2800695547973644690809652`.
Stationary local point: `[666338534620.8127514365867464, 0.06127683692504237722431525981, 0.9881017912913879704147192181]`; logLik `128.2800695547973644690809652`. Verified coefficient relative error: `9.06e-08`.
Local information eigenvalue range: `[1.558347288902694209134674715, 444007042720612079107264.2006]`; per-observation stationarity residual `1.828755668356736021463060622e-47`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1876: frechet, constant-1.04938-1e-12

Data: `[1.0493786282149833, 1.0493786282160327, 1.049378628217082, 1.0493786282181314, 1.0493786282202302]`. Holds: `{'mu': 0.06127683692504238}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1876` (run through the slot wrapper).
Returned: `{'lambda': '666338474224.99927', 'mu': '0.061276836925042377', 'sigma': '0.98810179129138798'}`, status `0`, logLik `128.280069512836462308769114315509796142578125`.
Independent logLik at that point: `128.2800695547576467323422921` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `9.06e-08`.
Better feasible point: `[666338534620.8127514365867464, 0.06127683692504237722431525981, 0.9881017912913879704147192181]`; logLik `128.2800695547973644690809652`.
Stationary local point: `[666338534620.8127514365867464, 0.06127683692504237722431525981, 0.9881017912913879704147192181]`; logLik `128.2800695547973644690809652`. Verified coefficient relative error: `9.06e-08`.
Local information eigenvalue range: `[1.558347288902694209134674715, 444007042720612079107264.2006]`; per-observation stationarity residual `1.828755668356736021463060622e-47`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1881: frechet, offset-1e8

Data: `[100000001.04937863, 100000000.85214412, 100000000.72254047, 100000001.14423211, 100000000.8365356, 100000001.4870096, 100000000.94169319, 100000003.51675779, 100000000.93836436, 100000000.87958795, 100000001.51237431, 100000004.0291135]`. Holds: `{'mu': 0.06127683692504238}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1881` (run through the slot wrapper).
Returned: `{'lambda': '182713899.71233225', 'mu': '0.061276836925042377', 'sigma': '100000001.02259125'}`, status `0`, logLik `-13.726017904237238553832867182791233062744140625`.
Independent logLik at that point: `-13.72601735963708445421778333` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `3.97e-08`.
Stationary local point: `[182713908.0108968169700923087, 0.06127683692504237722431525981, 100000001.0225914089984009427]`; logLik `-13.72601735963651045441632273`. Verified coefficient relative error: `—`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1907: frechet, constant-1.04938-0.0001

Data: `[1.0491687524914386, 1.0492736903542603, 1.049378628217082, 1.0494835660799038, 1.0496934418055472]`. Holds: `{'sigma': 0.9001111496659718}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1907` (run through the slot wrapper).
Returned: `{'lambda': '6070.4386677779839', 'mu': '0.14920229500596316', 'sigma': '0.90011114966597183'}`, status `0`, logLik `36.17665438372339536954314098693430423736572265625`.
Independent logLik at that point: `36.17665438372344571439562336` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.62e-08`.
Better feasible point: `[6070.438569260160098407977497, 0.1492022950158745744780593117, 0.9001111496659718325119570181]`; logLik `36.17665438372345575360600112`.
Stationary local point: `[6070.438569260160098407977497, 0.1492022950158745744780593117, 0.9001111496659718325119570181]`; logLik `36.17665438372345575360600112`. Verified coefficient relative error: `1.62e-08`.
Local information eigenvalue range: `[1.533675257554756528139655788, 12.72342277419149751909000397]`; per-observation stationarity residual `4.120000808466633083507000651e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1908: frechet, constant-1.04938-1e-08

Data: `[1.0493786072295095, 1.0493786177232958, 1.049378628217082, 1.0493786387108683, 1.049378659698441]`. Holds: `{'sigma': 0.9001111496659718}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1908` (run through the slot wrapper).
Returned: `{'lambda': '60719489.333490357', 'mu': '0.14926747201465929', 'sigma': '0.90011114966597183'}`, status `0`, logLik `82.228383733122200283105485141277313232421875`.
Independent logLik at that point: `82.22838373210824929277765479` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `4.68e-08`.
Better feasible point: `[60700338.73794148793144747737, 0.1492674720340672163219867583, 0.9001111496659718325119570181]`; logLik `82.22838758179598539706906791`.
Stationary local point: `[60700338.73794148793144747737, 0.1492674720340672163219867583, 0.9001111496659718325119570181]`; logLik `82.22838758179598539706906791`. Verified coefficient relative error: `—`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1909: frechet, constant-1.04938-1e-12

Data: `[1.0493786282149833, 1.0493786282160327, 1.049378628217082, 1.0493786282181314, 1.0493786282202302]`. Holds: `{'sigma': 0.9001111496659718}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1909` (run through the slot wrapper).
Returned: `{'lambda': '480983720672.12512', 'mu': '0.14926747854901146', 'sigma': '0.90011114966597183'}`, status `0`, logLik `126.7103282180471666151788667775690555572509765625`.
Independent logLik at that point: `126.7103350118462007011301719` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.000625`.
Better feasible point: `[480983720672.1251220703125, 0.1492674785490639255414180298, 0.9001111496659718325119570181]`; logLik `126.789584527936635211846333`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1910: frechet, constant-1.04938-0.0001

Data: `[1.0491687524914386, 1.0492736903542603, 1.049378628217082, 1.0494835660799038, 1.0496934418055472]`. Holds: `{'sigma': 0.9001111496659718}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1910` (run through the slot wrapper).
Returned: `{'lambda': '6070.4386677779839', 'mu': '0.14920229500596316', 'sigma': '0.90011114966597183'}`, status `0`, logLik `36.17665438372339536954314098693430423736572265625`.
Independent logLik at that point: `36.17665438372344571439562336` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.62e-08`.
Better feasible point: `[6070.438569260160098407977497, 0.1492022950158745744780593117, 0.9001111496659718325119570181]`; logLik `36.17665438372345575360600112`.
Stationary local point: `[6070.438569260160098407977497, 0.1492022950158745744780593117, 0.9001111496659718325119570181]`; logLik `36.17665438372345575360600112`. Verified coefficient relative error: `1.62e-08`.
Local information eigenvalue range: `[1.533675257554756528139655788, 12.72342277419149751909000397]`; per-observation stationarity residual `4.120000808466633083507000651e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1911: frechet, constant-1.04938-1e-08

Data: `[1.0493786072295095, 1.0493786177232958, 1.049378628217082, 1.0493786387108683, 1.049378659698441]`. Holds: `{'sigma': 0.9001111496659718}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1911` (run through the slot wrapper).
Returned: `{'lambda': '60719489.333490357', 'mu': '0.14926747201465929', 'sigma': '0.90011114966597183'}`, status `0`, logLik `82.228383733122200283105485141277313232421875`.
Independent logLik at that point: `82.22838373210824929277765479` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `4.68e-08`.
Better feasible point: `[60700338.73794148793144747737, 0.1492674720340672163219867583, 0.9001111496659718325119570181]`; logLik `82.22838758179598539706906791`.
Stationary local point: `[60700338.73794148793144747737, 0.1492674720340672163219867583, 0.9001111496659718325119570181]`; logLik `82.22838758179598539706906791`. Verified coefficient relative error: `—`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1912: frechet, constant-1.04938-1e-12

Data: `[1.0493786282149833, 1.0493786282160327, 1.049378628217082, 1.0493786282181314, 1.0493786282202302]`. Holds: `{'sigma': 0.9001111496659718}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1912` (run through the slot wrapper).
Returned: `{'lambda': '480983720672.12512', 'mu': '0.14926747854901146', 'sigma': '0.90011114966597183'}`, status `0`, logLik `126.7103282180471666151788667775690555572509765625`.
Independent logLik at that point: `126.7103350118462007011301719` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.000625`.
Better feasible point: `[480983720672.1251220703125, 0.1492674785490639255414180298, 0.9001111496659718325119570181]`; logLik `126.789584527936635211846333`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1913: frechet, lower-1e-06

Data: `[0.06127783692504238, 0.06127883692504238, 0.06127983692504238, 0.061281836925042375, 0.06128583692504238]`. Holds: `{'sigma': 0.9001111496659718}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1913` (run through the slot wrapper).
Returned: `{'lambda': '442816.17135617073', 'mu': '-0.83883159208052793', 'sigma': '0.90011114966597183'}`, status `0`, logLik `57.3838830641419690437032841145992279052734375`.
Independent logLik at that point: `57.38388306413656798375986034` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.3e-06`.
Better feasible point: `[442815.1527421623146198200367, -0.8388315920610721815385544914, 0.9001111496659718325119570181]`; logLik `57.38388306434519496915685047`.
Stationary local point: `[442815.1527421623146198200367, -0.8388315920610721815385544914, 0.9001111496659718325119570181]`; logLik `57.38388306434519496915685047`. Verified coefficient relative error: `2.3e-06`.
Local information eigenvalue range: `[1.391197599292824204083532903, 15.64028396832322376384940887]`; per-observation stationarity residual `1.350320807319745771361452384e-41`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1914: frechet, lower-1e-12

Data: `[0.061276836926042376, 0.061276836927042375, 0.06127683692804238, 0.06127683693004238, 0.06127683693404238]`. Holds: `{'sigma': 0.9001111496659718}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1914` (run through the slot wrapper).
Returned: `{'lambda': '364712651040.33655', 'mu': '-0.83883431273992948', 'sigma': '0.90011114966597183'}`, status `0`, logLik `125.211822724039251397698535583913326263427734375`.
Independent logLik at that point: `125.2118426749454127455280269` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.00998`.
Better feasible point: `[442814147414.0427837255521632, -0.8388343127382087732548616379, 0.9001111496659718325119570181]`; logLik `126.4614301232779823259653635`.
Stationary local point: `[442814147414.0427837255521632, -0.8388343127382087732548616379, 0.9001111496659718325119570181]`; logLik `126.4614301232779823259653635`. Verified coefficient relative error: `—`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1920: frechet, limit-concentration

Data: `[1.0493765294598256, 1.0493775788384538, 1.049378628217082, 1.0493796775957103, 1.0493807269743385]`. Holds: `{'sigma': 0.9001111496659718}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1920` (run through the slot wrapper).
Returned: `{'lambda': '677184.08215357969', 'mu': '0.14926674042058696', 'sigma': '0.90011114966597183'}`, status `0`, logLik `59.87809198261169285615324042737483978271484375`.
Independent logLik at that point: `59.87809198261494682790391898` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `8.68e-07`.
Better feasible point: `[677183.4941920121861680960113, 0.1492667404253481541598673757, 0.9001111496659718325119570181]`; logLik `59.87809198264356629093765664`.
Stationary local point: `[677183.4941920121861680960113, 0.1492667404253481541598673757, 0.9001111496659718325119570181]`; logLik `59.87809198264356629093765664`. Verified coefficient relative error: `8.68e-07`.
Local information eigenvalue range: `[1.599111170297897663293791211, 10.20804333898721429501405604]`; per-observation stationarity residual `1.890344240508445945081339082e-45`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 1993: gamma, limit-outlier

Data: `[1.0321428726197281, 0.20581009427807215, 1.294604726247099, 1.1511176265150493, 1032144.6469452523]`. Holds: `{}`.
Call: `route_gamma(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 1993` (run through the slot wrapper).
Returned: `{'shape': '0.085366463604193935', 'scale': '2418158.8109498867'}`, status `0`, logLik `-30.40009166619653768748321454040706157684326171875`.
Independent logLik at that point: `-30.40009166619653782775763297` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.08e-08`.
Better feasible point: `[0.08536646879862489773813861148, 2418158.663808284531647214406]`; logLik `-30.40009166619652926300448509`.
Stationary local point: `[0.08536646879862489773813861148, 2418158.663808284531647214406]`; logLik `-30.40009166619652926300448509`. Verified coefficient relative error: `6.08e-08`.
Local information eigenvalue range: `[0.0775564826708773056311752829, 1.018457035584989001671187601]`; per-observation stationarity residual `2.654884273731435511484653979e-58`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gamma/mle.hpp:71` (special difference): `return std::log(k) - cppunivariate::special::digamma(k);`. Execution-path causation remains open.

## Case 2101: gb2, lower-1e-06

Data: `[1e-06, 2e-06, 3e-06, 4.9999999999999996e-06, 9e-06]`. Holds: `{'shape2': 1.123220989970799}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2101` (run through the slot wrapper).
Returned: `{'shape1': '5.9127804053239101', 'shape2': '1.123220989970799', 'shape3': '2.2022498593715123', 'scale': '8.4885115116203619e-06'}`, status `0`, logLik `57.73596208254365791390227968804538249969482421875`.
Independent logLik at that point: `57.73596208254363304439327096` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.64e-08`.
Better feasible point: `[5.912780527822013560530088851, 1.123220989970798999735279722, 2.202249846954210137088374382, 0.000008488511735867830900136042697]`; logLik `57.7359620825436330497438081`.
Stationary local point: `[5.912780527822013560530088851, 1.123220989970798999735279722, 2.202249846954210137088374382, 0.000008488511735867830900136042697]`; logLik `57.7359620825436330497438081`. Verified coefficient relative error: `2.64e-08`.
Local information eigenvalue range: `[0.001214463649940765510598499191, 5.472621151164593944935605868]`; per-observation stationarity residual `2.919465169921995593700433595e-56`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 2102: gb2, lower-1e-12

Data: `[1e-12, 2e-12, 3e-12, 5e-12, 9e-12]`. Holds: `{'shape2': 1.123220989970799}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2102` (run through the slot wrapper).
Returned: `{'shape1': '5.9127796554182401', 'shape2': '1.123220989970799', 'shape3': '2.2022499437370815', 'scale': '8.4885101027211259e-12'}`, status `0`, logLik `126.81351487236503317035385407507419586181640625`.
Independent logLik at that point: `126.8135148723650035223074005` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.92e-07`.
Better feasible point: `[5.912780527822025410343551543, 1.123220989970798999735279722, 2.2022498469542088251673907, 8.488511735867853124750308451e-12]`; logLik `126.8135148723650037441760345`.
Stationary local point: `[5.912780527822025410343551543, 1.123220989970798999735279722, 2.2022498469542088251673907, 8.488511735867853124750308451e-12]`; logLik `126.8135148723650037441760345`. Verified coefficient relative error: `1.92e-07`.
Local information eigenvalue range: `[0.001214463298780015480546805241, 5.472620269023780217643177174]`; per-observation stationarity residual `1.606259960494270455053327013e-49`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 2109: gb2, interior

Data: `[57.4353411997682, 0.2838470478695372, 18.664516390732928, 3.6293660603679108, 3.5222145267466876, 66.57936349032846, 0.351786361375387, 1.9334530814511586, 12.118836860348935, 2.5345247148741246, 102.88577151817019, 2.157820291412655]`. Holds: `{'shape1': 0.5710330324328029, 'shape2': 1.123220989970799}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2109` (run through the slot wrapper).
Returned: `{'shape1': '0.5710330324328029', 'shape2': '1.123220989970799', 'shape3': '1.0741923425581008', 'scale': '2.041411244921381'}`, status `0`, logLik `-45.9227829556482873840650427155196666717529296875`.
Independent logLik at that point: `-45.92278295564833711410232536` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.84e-08`.
Better feasible point: `[0.5710330324328029005798157414, 1.123220989970798999735279722, 1.074192355598148500443015807, 2.041411207335534535744722737]`; logLik `-45.92278295564833699729513254`.
Stationary local point: `[0.5710330324328029005798157414, 1.123220989970798999735279722, 1.074192355598148500443015807, 2.041411207335534535744722737]`; logLik `-45.92278295564833699729513254`. Verified coefficient relative error: `1.84e-08`.
Local information eigenvalue range: `[0.03707002267915284722603346275, 0.9999395046886332476695712558]`; per-observation stationarity residual `2.950478638238358473496801562e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 2113: gb2, ties

Data: `[57.4353411997682, 57.4353411997682, 57.4353411997682, 57.4353411997682, 0.2838470478695372, 18.664516390732928]`. Holds: `{'shape1': 0.5710330324328029, 'shape2': 1.123220989970799}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2113` (run through the slot wrapper).
Returned: `{'shape1': '0.5710330324328029', 'shape2': '1.123220989970799', 'shape3': '0.53959600609531344', 'scale': '33.991081478476652'}`, status `0`, logLik `-30.18797136266557146200284478254616260528564453125`.
Independent logLik at that point: `-30.187971362665605565161443` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.39e-08`.
Better feasible point: `[0.5710330324328029005798157414, 1.123220989970798999735279722, 0.5395959947550878040947908691, 33.99108228956722027429980726]`; logLik `-30.18797136266560491753947387`.
Stationary local point: `[0.5710330324328029005798157414, 1.123220989970798999735279722, 0.5395959947550878040947908691, 33.99108228956722027429980726]`; logLik `-30.18797136266560491753947387`. Verified coefficient relative error: `2.39e-08`.
Local information eigenvalue range: `[0.1325779938247892991959544161, 0.9864108612376358626328792867]`; per-observation stationarity residual `6.589934213282822488962426862e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 2154: gb2, constant-3.62937-1e-12

Data: `[3.629366060360652, 3.629366060364281, 3.6293660603679108, 3.6293660603715403, 3.629366060378799]`. Holds: `{'shape1': 0.5710330324328029, 'shape3': 0.7116483765738807}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2154` (run through the slot wrapper).
Returned: `{'shape1': '0.5710330324328029', 'shape2': '1377154191189.8601', 'shape3': '0.71164837657388069', 'scale': '3.629366060366976'}`, status `0`, logLik `121.72895278381241723764105699956417083740234375`.
Independent logLik at that point: `121.7289966466047468032457008` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `3.6e-07`.
Stationary local point: `[0.5710330324328029005798157414, 1377168712014.52165526982094, 0.7116483765738806921419268292, 3.629366060366975793565133834]`; logLik `121.7289966485131810203963752`. Verified coefficient relative error: `—`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 2157: gb2, constant-3.62937-1e-12

Data: `[3.629366060360652, 3.629366060364281, 3.6293660603679108, 3.6293660603715403, 3.629366060378799]`. Holds: `{'shape1': 0.5710330324328029, 'shape3': 0.7116483765738807}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2157` (run through the slot wrapper).
Returned: `{'shape1': '0.5710330324328029', 'shape2': '1377154191189.8601', 'shape3': '0.71164837657388069', 'scale': '3.629366060366976'}`, status `0`, logLik `121.72895278381241723764105699956417083740234375`.
Independent logLik at that point: `121.7289966466047468032457008` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `3.6e-07`.
Stationary local point: `[0.5710330324328029005798157414, 1377168712014.52165526982094, 0.7116483765738806921419268292, 3.629366060366975793565133834]`; logLik `121.7289966485131810203963752`. Verified coefficient relative error: `—`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 2181: gb2, scale-1e-150

Data: `[5.74353411997682e-149, 2.8384704786953717e-151, 1.8664516390732927e-149, 3.629366060367911e-150, 3.5222145267466875e-150, 6.657936349032846e-149, 3.51786361375387e-151, 1.9334530814511585e-150, 1.2118836860348935e-149, 2.5345247148741247e-150, 1.028857715181702e-148, 2.157820291412655e-150]`. Holds: `{'shape2': 1.123220989970799, 'shape3': 0.7116483765738807}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2181` (run through the slot wrapper).
Returned: `{'shape1': '0.71751554291624042', 'shape2': '1.123220989970799', 'shape3': '0.71164837657388069', 'scale': '5.5056513782722311e-150'}`, status `0`, logLik `4098.65599962961277924478054046630859375`.
Independent logLik at that point: `4098.655999629612961156338681` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.47e-08`.
Better feasible point: `[0.7175155490696979734894159638, 1.123220989970798999735279722, 0.7116483765738806921419268292, 5.505651459097354699714610959e-150]`; logLik `4098.655999629612961262429476`.
Stationary local point: `[0.7175155490696979734894159638, 1.123220989970798999735279722, 0.7116483765738806921419268292, 5.505651459097354699714610959e-150]`; logLik `4098.655999629612961262429476`. Verified coefficient relative error: `1.47e-08`.
Local information eigenvalue range: `[0.05448298403967633430431858316, 1.082432052980337591902617447]`; per-observation stationarity residual `4.888849217288808904564613291e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 2182: gb2, scale-1e+150

Data: `[5.743534119976821e+151, 2.8384704786953718e+149, 1.8664516390732927e+151, 3.6293660603679105e+150, 3.522214526746688e+150, 6.657936349032846e+151, 3.5178636137538697e+149, 1.9334530814511586e+150, 1.2118836860348935e+151, 2.5345247148741246e+150, 1.0288577151817018e+152, 2.1578202914126548e+150]`. Holds: `{'shape2': 1.123220989970799, 'shape3': 0.7116483765738807}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2182` (run through the slot wrapper).
Returned: `{'shape1': '0.71751554291623443', 'shape2': '1.123220989970799', 'shape3': '0.71164837657388069', 'scale': '5.5056513782721002e+150'}`, status `0`, logLik `-4190.650335148951853625476360321044921875`.
Independent logLik at that point: `-4190.650335148951501212260946` (232 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.47e-08`.
Better feasible point: `[0.7175155490696979742950014977, 1.123220989970798999735279722, 0.7116483765738806921419268292, 5.505651459097354673463876734e+150]`; logLik `-4190.650335148951501106169923`.
Stationary local point: `[0.7175155490696979742950014977, 1.123220989970798999735279722, 0.7116483765738806921419268292, 5.505651459097354673463876734e+150]`; logLik `-4190.650335148951501106169923`. Verified coefficient relative error: `1.47e-08`.
Local information eigenvalue range: `[0.05448298403967405832518132257, 1.082432052980313245447275537]`; per-observation stationarity residual `4.888920993008187387619626817e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 2215: gb2, lower-1e-06

Data: `[1e-06, 2e-06, 3e-06, 4.9999999999999996e-06, 9e-06]`. Holds: `{'scale': 1.0079365599415882}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2215` (run through the slot wrapper).
Returned: `{'shape1': '597.10004129450488', 'shape2': '0.19427428175172309', 'shape3': '51.063120626905551', 'scale': '1.0079365599415882'}`, status `0`, logLik `57.803099374535207743974751792848110198974609375`.
Independent logLik at that point: `57.80309937453500878015897192` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.86e-07`.
Better feasible point: `[597.1000073450689812269091568, 0.1942742306614380515943702676, 51.0631505684620023168787487, 1.007936559941588150479674368]`; logLik `57.8030993745350093129355107`.
Stationary local point: `[597.1000073450689812269091568, 0.1942742306614380515943702676, 51.0631505684620023168787487, 1.007936559941588150479674368]`; logLik `57.8030993745350093129355107`. Verified coefficient relative error: `5.86e-07`.
Local information eigenvalue range: `[0.0003021548644270327755692166307, 378.2249764522210826028546163]`; per-observation stationarity residual `4.849998879016827464120993298e-68`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 2242: gb2, interior

Data: `[57.4353411997682, 0.2838470478695372, 18.664516390732928, 3.6293660603679108, 3.5222145267466876, 66.57936349032846, 0.351786361375387, 1.9334530814511586, 12.118836860348935, 2.5345247148741246, 102.88577151817019, 2.157820291412655]`. Holds: `{'shape2': 1.123220989970799, 'scale': 1.0079365599415882}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2242` (run through the slot wrapper).
Returned: `{'shape1': '0.51314124214135393', 'shape2': '1.123220989970799', 'shape3': '1.5123091765773569', 'scale': '1.0079365599415882'}`, status `0`, logLik `-46.00312462082298026189164374954998493194580078125`.
Independent logLik at that point: `-46.00312462082304619614890279` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.59e-08`.
Better feasible point: `[0.5131412376019535574699813911, 1.123220989970798999735279722, 1.512309137417201865106637639, 1.007936559941588150479674368]`; logLik `-46.00312462082304437760031714`.
Stationary local point: `[0.5131412376019535574699813911, 1.123220989970798999735279722, 1.512309137417201865106637639, 1.007936559941588150479674368]`; logLik `-46.00312462082304437760031714`. Verified coefficient relative error: `2.59e-08`.
Local information eigenvalue range: `[0.3354059920358744946527083857, 1.403773494623749812083720685]`; per-observation stationarity residual `2.210200121474151842389666057e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 2254: gb2, lower-1e-12

Data: `[1e-12, 2e-12, 3e-12, 5e-12, 9e-12]`. Holds: `{'shape2': 1.123220989970799, 'scale': 1.0079365599415882}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2254` (run through the slot wrapper).
Returned: `{'shape1': '10245575333448.246', 'shape2': '1.123220989970799', 'shape3': '1.6533828961377981', 'scale': '1.0079365599415882'}`, status `0`, logLik `126.8068557828784577168335090391337871551513671875`.
Independent logLik at that point: `126.8068557828779655807916046` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.46e-08`.
Better feasible point: `[10245574876671.09809735229707, 1.123220989970798999735279722, 1.653382828300373459653662863, 1.007936559941588150479674368]`; logLik `126.8068557828779681361159027`.
Stationary local point: `[10245574876671.09809735229707, 1.123220989970798999735279722, 1.653382828300373459653662863, 1.007936559941588150479674368]`; logLik `126.8068557828779681361159027`. Verified coefficient relative error: `4.46e-08`.
Local information eigenvalue range: `[0.2708550455030311380556364978, 3.630684978261353869957784654]`; per-observation stationarity residual `6.25854294458190199415773519e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 2264: gb2, tiny-5

Data: `[57.4353411997682, 0.2838470478695372, 18.664516390732928, 3.6293660603679108, 3.5222145267466876]`. Holds: `{'shape1': 0.5710330324328029, 'shape2': 1.123220989970799, 'scale': 1.0079365599415882}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2264` (run through the slot wrapper).
Returned: `{'shape1': '0.5710330324328029', 'shape2': '1.123220989970799', 'shape3': '1.5234422150143867', 'scale': '1.0079365599415882'}`, status `0`, logLik `-18.80974585565318335511619807220995426177978515625`.
Independent logLik at that point: `-18.80974585565320712705218604` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.5e-08`.
Better feasible point: `[0.5710330324328029005798157414, 1.123220989970798999735279722, 1.523442146508101814496667611, 1.007936559941588150479674368]`; logLik `-18.80974585565320352612675183`.
Stationary local point: `[0.5710330324328029005798157414, 1.123220989970798999735279722, 1.523442146508101814496667611, 1.007936559941588150479674368]`; logLik `-18.80974585565320352612675183`. Verified coefficient relative error: `4.5e-08`.
Local information eigenvalue range: `[0.7123038191813738318180416242, 0.7123038191813738318180416242]`; per-observation stationarity residual `2.054727468565627390663215015e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 2283: gb2, tiny-5

Data: `[57.4353411997682, 0.2838470478695372, 18.664516390732928, 3.6293660603679108, 3.5222145267466876]`. Holds: `{'shape3': 0.7116483765738807, 'scale': 1.0079365599415882}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2283` (run through the slot wrapper).
Returned: `{'shape1': '0.28568848710667422', 'shape2': '1.5943233513279058', 'shape3': '0.71164837657388069', 'scale': '1.0079365599415882'}`, status `0`, logLik `-19.0425600068967497691119206137955188751220703125`.
Independent logLik at that point: `-19.042560006896778834204809` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.11e-08`.
Better feasible point: `[0.2856884696632222368073033648, 1.594323412542879863854399989, 0.7116483765738806921419268292, 1.007936559941588150479674368]`; logLik `-19.04256000689677470702392491`.
Stationary local point: `[0.2856884696632222368073033648, 1.594323412542879863854399989, 0.7116483765738806921419268292, 1.007936559941588150479674368]`; logLik `-19.04256000689677470702392491`. Verified coefficient relative error: `6.11e-08`.
Local information eigenvalue range: `[0.2932734124302791076002897039, 1.971848190054967488683714507]`; per-observation stationarity residual `2.611054107574137119490813946e-57`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 2291: gb2, lower-1e-06

Data: `[1e-06, 2e-06, 3e-06, 4.9999999999999996e-06, 9e-06]`. Holds: `{'shape3': 0.7116483765738807, 'scale': 1.0079365599415882}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2291` (run through the slot wrapper).
Returned: `{'shape1': '4012873810.6008468', 'shape2': '1.8308504936087409', 'shape3': '0.71164837657388069', 'scale': '1.0079365599415882'}`, status `0`, logLik `57.6333142997319356481966678984463214874267578125`.
Independent logLik at that point: `57.63331429973202876092120468` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.68e-07`.
Better feasible point: `[4012874485.701960929514635562, 1.830850507639056611821901256, 0.7116483765738806921419268292, 1.007936559941588150479674368]`; logLik `57.63331429973202898983156354`.
Stationary local point: `[4012874485.701960929514635562, 1.830850507639056611821901256, 0.7116483765738806921419268292, 1.007936559941588150479674368]`; logLik `57.63331429973202898983156354`. Verified coefficient relative error: `1.68e-07`.
Local information eigenvalue range: `[0.003228483088976119546150873613, 342.1245899644614281050498222]`; per-observation stationarity residual `1.461586124744623451996072957e-49`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 2293: gb2, lower-1e-17

Data: `[1e-17, 2e-17, 3e-17, 5.0000000000000005e-17, 9.000000000000001e-17]`. Holds: `{'shape3': 0.7116483765738807, 'scale': 1.0079365599415882}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2293` (run through the slot wrapper).
Returned: `{'shape1': '5.5309923104107816e+29', 'shape2': '1.8308500070424825', 'shape3': '0.71164837657388069', 'scale': '1.0079365599415882'}`, status `0`, logLik `184.275494414421331157427630387246608734130859375`.
Independent logLik at that point: `184.2754944144215829263072418` (109 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.87e-05`.
Better feasible point: `[5.531095489573420565432624964e+29, 1.830850507404427217882315081, 0.7116483765738806921419268292, 1.007936559941588150479674368]`; logLik `184.2754944144218728767559566`.
Stationary local point: `[5.531095489573420565432624964e+29, 1.830850507404427217882315081, 0.7116483765738806921419268292, 1.007936559941588150479674368]`; logLik `184.2754944144218728767559566`. Verified coefficient relative error: `1.87e-05`.
Local information eigenvalue range: `[0.0003331996354134300453291946643, 3314.835631811580593538250705]`; per-observation stationarity residual `4.462630496479988232324337331e-66`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 2322: gb2, ties

Data: `[57.4353411997682, 57.4353411997682, 57.4353411997682, 57.4353411997682, 0.2838470478695372, 18.664516390732928]`. Holds: `{'shape2': 1.123220989970799, 'shape3': 0.7116483765738807, 'scale': 1.0079365599415882}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2322` (run through the slot wrapper).
Returned: `{'shape1': '0.24759398976095959', 'shape2': '1.123220989970799', 'shape3': '0.71164837657388069', 'scale': '1.0079365599415882'}`, status `0`, logLik `-32.89884200469601438499012147076427936553955078125`.
Independent logLik at that point: `-32.89884200469604081304551924` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.02e-08`.
Better feasible point: `[0.2475939872274554468774388311, 1.123220989970798999735279722, 0.7116483765738806921419268292, 1.007936559941588150479674368]`; logLik `-32.89884200469604050948742841`.
Stationary local point: `[0.2475939872274554468774388311, 1.123220989970798999735279722, 0.7116483765738806921419268292, 1.007936559941588150479674368]`; logLik `-32.89884200469604050948742841`. Verified coefficient relative error: `1.02e-08`.
Local information eigenvalue range: `[0.96640133358510833956153113, 0.96640133358510833956153113]`; per-observation stationarity residual `1.372299056114458884603873097e-64`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 2331: gb2, lower-1e-17

Data: `[1e-17, 2e-17, 3e-17, 5.0000000000000005e-17, 9.000000000000001e-17]`. Holds: `{'shape2': 1.123220989970799, 'shape3': 0.7116483765738807, 'scale': 1.0079365599415882}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2331` (run through the slot wrapper).
Returned: `{'shape1': '1.8219370529000558e+18', 'shape2': '1.123220989970799', 'shape3': '0.71164837657388069', 'scale': '1.0079365599415882'}`, status `0`, logLik `183.5183551180408585423720069229602813720703125`.
Independent logLik at that point: `183.5183551180409014245930027` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.07e-07`.
Better feasible point: `[1821936857250651589.213196237, 1.123220989970798999735279722, 0.7116483765738806921419268292, 1.007936559941588150479674368]`; logLik `183.5183551180409219407425877`.
Stationary local point: `[1821936857250651589.213196237, 1.123220989970798999735279722, 0.7116483765738806921419268292, 1.007936559941588150479674368]`; logLik `183.5183551180409219407425877`. Verified coefficient relative error: `1.07e-07`.
Local information eigenvalue range: `[0.7116485294151691394073574571, 0.7116485294151691394073574571]`; per-observation stationarity residual `3.110511956486113070898888977e-56`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 2372: ged, interior

Data: `[0.7321666961121761, 2.2766042947262384, -0.5491691631533211, 1.5779966110538917, -0.25578223779382836, -0.021339634443158128, -1.1263577338146473, -0.8880300857748882, -0.8586426179623097, 0.32437851494901027, 0.40891269493055626, -0.43403506191995683]`. Holds: `{'mean': -0.017698887677594307}`.
Call: `route_ged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2372` (run through the slot wrapper).
Returned: `{'mean': '-0.017698887677594307', 'sd': '0.99472226277241416', 'nu': '1.7774977947805037'}`, status `0`, logLik `-16.94575870602466238779015839099884033203125`.
Independent logLik at that point: `-16.94575870602466587095570495` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3e-08`.
Better feasible point: `[-0.01769888767759430700521328106, 0.9947222635584589676959405253, 1.777497741433200539495872371]`; logLik `-16.94575870602466456008002557`.
Stationary local point: `[-0.01769888767759430700521328106, 0.9947222635584589676959405253, 1.777497741433200539495872371]`; logLik `-16.94575870602466456008002557`. Verified coefficient relative error: `3e-08`.
Local information eigenvalue range: `[0.2423566880342277847350742352, 1.778924509748510006551210957]`; per-observation stationarity residual `8.538551378693517183316573376e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp:219` (step stop): `inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {`. Execution-path causation remains open.

## Case 2384: ged, scale-1e+150

Data: `[7.321666961121761e+149, 2.276604294726238e+150, -5.491691631533211e+149, 1.5779966110538917e+150, -2.5578223779382837e+149, -2.1339634443158128e+148, -1.1263577338146473e+150, -8.880300857748881e+149, -8.586426179623096e+149, 3.2437851494901025e+149, 4.0891269493055628e+149, -4.340350619199568e+149]`. Holds: `{'mean': -0.017698887677594307}`.
Call: `route_ged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2384` (run through the slot wrapper).
Returned: `{'mean': '-0.017698887677594307', 'sd': '9.9287276788875443e+149', 'nu': '1.829955432298777'}`, status `0`, logLik `-4161.5845528488544005085714161396026611328125`.
Independent logLik at that point: `-4161.584552848854771365259753` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.03e-07`.
Better feasible point: `[-0.01769888767759430700521328106, 9.928727736983063209065392829e+149, 1.829954878539116603967086438]`; logLik `-4161.584552848854639630343718`.
Stationary local point: `[-0.01769888767759430700521328106, 9.928727736983063209065392829e+149, 1.829954878539116603967086438]`; logLik `-4161.584552848854639630343718`. Verified coefficient relative error: `3.03e-07`.
Local information eigenvalue range: `[0.2396632148426335832833253394, 1.830742163556712372887529908]`; per-observation stationarity residual `1.109132376502860734380745122e-51`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp:219` (step stop): `inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {`. Execution-path causation remains open.

## Case 2388: ged, interior

Data: `[0.7321666961121761, 2.2766042947262384, -0.5491691631533211, 1.5779966110538917, -0.25578223779382836, -0.021339634443158128, -1.1263577338146473, -0.8880300857748882, -0.8586426179623097, 0.32437851494901027, 0.40891269493055626, -0.43403506191995683]`. Holds: `{'sd': 1.0178559902092952}`.
Call: `route_ged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2388` (run through the slot wrapper).
Returned: `{'mean': '0.5751232804557953', 'sd': '1.0178559902092952', 'nu': '267.11640593436084'}`, status `0`, logLik `-15.121495715294468453748777392320334911346435546875`.
Independent logLik at that point: `-15.12149571529447374090606309` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.92e-07`.
Better feasible point: `[0.5751232804557955269214707496, 1.01785599020929518054856544, 267.1163546417810071668690296]`; logLik `-15.12149571529447364237819953`.
Stationary local point: `[0.5751232804557955269214707496, 1.01785599020929518054856544, 267.1163546417810071668690296]`; logLik `-15.12149571529447364237819953`. Verified coefficient relative error: `1.92e-07`.
Local information eigenvalue range: `[0.0004453487001828255256108156547, 2.04842793716005491495276715]`; per-observation stationarity residual `2.352521130052969149813458357e-52`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp:219` (step stop): `inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {`. Execution-path causation remains open.

## Case 2389: ged, tiny-2

Data: `[0.7321666961121761, 2.2766042947262384]`. Holds: `{'sd': 1.0178559902092952}`.
Call: `route_ged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2389` (run through the slot wrapper).
Returned: `{'mean': '1.5043854954192071', 'sd': '1.0178559902092952', 'nu': '3.2844816440993156'}`, status `0`, logLik `-2.37796331278265693498497057589702308177947998046875`.
Independent logLik at that point: `-2.377963312782657652402311572` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.81e-08`.
Better feasible point: `[1.504385495419207241507564277, 1.01785599020929518054856544, 3.284481769075351500479990638]`; logLik `-2.377963312782657327370304074`.
Stationary local point: `[1.504385495419207241507564277, 1.01785599020929518054856544, 3.284481769075351500479990638]`; logLik `-2.377963312782657327370304074`. Verified coefficient relative error: `3.81e-08`.
Local information eigenvalue range: `[0.2244948028243444504780648481, 2.254907628921742729077055431]`; per-observation stationarity residual `5.141707330736012258933057497e-58`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp:219` (step stop): `inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {`. Execution-path causation remains open.

## Case 2390: ged, tiny-3

Data: `[0.7321666961121761, 2.2766042947262384, -0.5491691631533211]`. Holds: `{'sd': 1.0178559902092952}`.
Call: `route_ged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2390` (run through the slot wrapper).
Returned: `{'mean': '0.86371756578645864', 'sd': '1.0178559902092952', 'nu': '27.497341211256089'}`, status `0`, logLik `-3.774165366609527172414573215064592659473419189453125`.
Independent logLik at that point: `-3.774165366609527987595683765` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.67e-07`.
Better feasible point: `[0.8637175657864586408862805911, 1.01785599020929518054856544, 27.49733661683168433866852527]`; logLik `-3.774165366609527166575071394`.
Stationary local point: `[0.8637175657864586408862805911, 1.01785599020929518054856544, 27.49733661683168433866852527]`; logLik `-3.774165366609527166575071394`. Verified coefficient relative error: `1.67e-07`.
Local information eigenvalue range: `[0.01960569261686808244415012216, 2.77570675478143523377721968]`; per-observation stationarity residual `2.683275705464172130449998928e-52`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp:219` (step stop): `inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {`. Execution-path causation remains open.

## Case 2391: ged, tiny-5

Data: `[0.7321666961121761, 2.2766042947262384, -0.5491691631533211, 1.5779966110538917, -0.25578223779382836]`. Holds: `{'sd': 1.0178559902092952}`.
Call: `route_ged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2391` (run through the slot wrapper).
Returned: `{'mean': '0.86352652228079552', 'sd': '1.0178559902092952', 'nu': '23.000295914256753'}`, status `0`, logLik `-6.28715560678576235176251429948024451732635498046875`.
Independent logLik at that point: `-6.287155606785765040826765316` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.65e-08`.
Better feasible point: `[0.8635265223617799692462596535, 1.01785599020929518054856544, 23.00029744288215509770167503]`; logLik `-6.287155606785764821427049448`.
Stationary local point: `[0.8635265223617799692462596535, 1.01785599020929518054856544, 23.00029744288215509770167503]`; logLik `-6.287155606785764821427049448`. Verified coefficient relative error: `6.65e-08`.
Local information eigenvalue range: `[0.01986829520722836158879354203, 3.207634182496687503015191695]`; per-observation stationarity residual `6.045780519302033554581783973e-56`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp:219` (step stop): `inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {`. Execution-path causation remains open.

## Case 2404: ged, interior

Data: `[0.7321666961121761, 2.2766042947262384, -0.5491691631533211, 1.5779966110538917, -0.25578223779382836, -0.021339634443158128, -1.1263577338146473, -0.8880300857748882, -0.8586426179623097, 0.32437851494901027, 0.40891269493055626, -0.43403506191995683]`. Holds: `{'mean': -0.017698887677594307, 'sd': 1.0178559902092952}`.
Call: `route_ged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2404` (run through the slot wrapper).
Returned: `{'mean': '-0.017698887677594307', 'sd': '1.0178559902092952', 'nu': '1.7657098936656996'}`, status `0`, logLik `-16.95125926756026757402651128359138965606689453125`.
Independent logLik at that point: `-16.95125926756027021628889575` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.69e-08`.
Better feasible point: `[-0.01769888767759430700521328106, 1.01785599020929518054856544, 1.765709863841559384477663413]`; logLik `-16.95125926756026982663562568`.
Stationary local point: `[-0.01769888767759430700521328106, 1.01785599020929518054856544, 1.765709863841559384477663413]`; logLik `-16.95125926756026982663562568`. Verified coefficient relative error: `1.69e-08`.
Local information eigenvalue range: `[0.2276302203177091170744162864, 0.2276302203177091170744162864]`; per-observation stationarity residual `9.939703844393138147779637081e-62`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp:219` (step stop): `inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {`. Execution-path causation remains open.

## Case 2405: ged, tiny-2

Data: `[0.7321666961121761, 2.2766042947262384]`. Holds: `{'mean': -0.017698887677594307, 'sd': 1.0178559902092952}`.
Call: `route_ged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2405` (run through the slot wrapper).
Returned: `{'mean': '-0.017698887677594307', 'sd': '1.0178559902092952', 'nu': '1.879668336860473'}`, status `0`, logLik `-4.681947342208037099453576956875622272491455078125`.
Independent logLik at that point: `-4.68194734220803832533477459` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.76e-08`.
Better feasible point: `[-0.01769888767759430700521328106, 1.01785599020929518054856544, 1.8796682661972528560774003]`; logLik `-4.681947342208037214278016081`.
Stationary local point: `[-0.01769888767759430700521328106, 1.01785599020929518054856544, 1.8796682661972528560774003]`; logLik `-4.681947342208037214278016081`. Verified coefficient relative error: `3.76e-08`.
Local information eigenvalue range: `[0.7861616606711555895399246667, 0.7861616606711555895399246667]`; per-observation stationarity residual `5.9851538058476424141987909e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp:219` (step stop): `inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {`. Execution-path causation remains open.

## Case 2406: ged, tiny-3

Data: `[0.7321666961121761, 2.2766042947262384, -0.5491691631533211]`. Holds: `{'mean': -0.017698887677594307, 'sd': 1.0178559902092952}`.
Call: `route_ged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2406` (run through the slot wrapper).
Returned: `{'mean': '-0.017698887677594307', 'sd': '1.0178559902092952', 'nu': '1.809763230668274'}`, status `0`, logLik `-5.74868963713591796249602339230477809906005859375`.
Independent logLik at that point: `-5.74868963713591879131812756` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.25e-08`.
Better feasible point: `[-0.01769888767759430700521328106, 1.01785599020929518054856544, 1.809763189893933154871037899]`; logLik `-5.74868963713591832484620351`.
Stationary local point: `[-0.01769888767759430700521328106, 1.01785599020929518054856544, 1.809763189893933154871037899]`; logLik `-5.74868963713591832484620351`. Verified coefficient relative error: `2.25e-08`.
Local information eigenvalue range: `[0.6126379289932579222363654267, 0.6126379289932579222363654267]`; per-observation stationarity residual `3.076443166762627377751211542e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp:219` (step stop): `inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {`. Execution-path causation remains open.

## Case 2407: ged, tiny-5

Data: `[0.7321666961121761, 2.2766042947262384, -0.5491691631533211, 1.5779966110538917, -0.25578223779382836]`. Holds: `{'mean': -0.017698887677594307, 'sd': 1.0178559902092952}`.
Call: `route_ged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2407` (run through the slot wrapper).
Returned: `{'mean': '-0.017698887677594307', 'sd': '1.0178559902092952', 'nu': '1.9581536536126012'}`, status `0`, logLik `-8.887036324983963453405522159300744533538818359375`.
Independent logLik at that point: `-8.88703632498396621222875162` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.55e-08`.
Better feasible point: `[-0.01769888767759430700521328106, 1.01785599020929518054856544, 1.958153683975423826289392433]`; logLik `-8.887036324983965985370515924`.
Stationary local point: `[-0.01769888767759430700521328106, 1.01785599020929518054856544, 1.958153683975423826289392433]`; logLik `-8.887036324983965985370515924`. Verified coefficient relative error: `1.55e-08`.
Local information eigenvalue range: `[0.3774190212852940109580628147, 0.3774190212852940109580628147]`; per-observation stationarity residual `1.824955701695594238962199246e-62`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp:219` (step stop): `inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {`. Execution-path causation remains open.

## Case 2408: ged, ties

Data: `[0.7321666961121761, 0.7321666961121761, 0.7321666961121761, 0.7321666961121761, 2.2766042947262384, -0.5491691631533211]`. Holds: `{'mean': -0.017698887677594307, 'sd': 1.0178559902092952}`.
Call: `route_ged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2408` (run through the slot wrapper).
Returned: `{'mean': '-0.017698887677594307', 'sd': '1.0178559902092952', 'nu': '2.1470530101873866'}`, status `0`, logLik `-9.374252288953368150714595685712993144989013671875`.
Independent logLik at that point: `-9.374252288953370485384994478` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.17e-08`.
Better feasible point: `[-0.01769888767759430700521328106, 1.01785599020929518054856544, 2.147052985076340779441898738]`; logLik `-9.374252288953370274212268351`.
Stationary local point: `[-0.01769888767759430700521328106, 1.01785599020929518054856544, 2.147052985076340779441898738]`; logLik `-9.374252288953370274212268351`. Verified coefficient relative error: `1.17e-08`.
Local information eigenvalue range: `[0.5146032330031076121578753378, 0.5146032330031076121578753378]`; per-observation stationarity residual `2.41577245174405659385571892e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp:219` (step stop): `inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {`. Execution-path causation remains open.

## Case 2412: ged, constant-1-0.0001

Data: `[0.9998, 0.9999, 1.0, 1.0001, 1.0003]`. Holds: `{'mean': -0.017698887677594307, 'sd': 1.0178559902092952}`.
Call: `route_ged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2412` (run through the slot wrapper).
Returned: `{'mean': '-0.017698887677594307', 'sd': '1.0178559902092952', 'nu': '7.1582344556483104'}`, status `0`, logLik `-6.1907921437201220982160521089099347591400146484375`.
Independent logLik at that point: `-6.190792143720123387046644352` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.62e-08`.
Better feasible point: `[-0.01769888767759430700521328106, 1.01785599020929518054856544, 7.1582340536528755522320031]`; logLik `-6.190792143720122487881811662`.
Stationary local point: `[-0.01769888767759430700521328106, 1.01785599020929518054856544, 7.1582340536528755522320031]`; logLik `-6.190792143720122487881811662`. Verified coefficient relative error: `5.62e-08`.
Local information eigenvalue range: `[0.1140430829260181118347212484, 0.1140430829260181118347212484]`; per-observation stationarity residual `2.75053879345515508331244002e-56`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp:219` (step stop): `inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {`. Execution-path causation remains open.

## Case 2413: ged, constant-1-1e-08

Data: `[0.99999998, 0.99999999, 1.0, 1.00000001, 1.00000003]`. Holds: `{'mean': -0.017698887677594307, 'sd': 1.0178559902092952}`.
Call: `route_ged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2413` (run through the slot wrapper).
Returned: `{'mean': '-0.017698887677594307', 'sd': '1.0178559902092952', 'nu': '7.1577709132690321'}`, status `0`, logLik `-6.19078078082745975052603171207010746002197265625`.
Independent logLik at that point: `-6.190780780827461798628812013` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.3e-08`.
Better feasible point: `[-0.01769888767759430700521328106, 1.01785599020929518054856544, 7.157771078034282698808410562]`; logLik `-6.190780780827461647546930395`.
Stationary local point: `[-0.01769888767759430700521328106, 1.01785599020929518054856544, 7.157771078034282698808410562]`; logLik `-6.190780780827461647546930395`. Verified coefficient relative error: `2.3e-08`.
Local information eigenvalue range: `[0.1140503700902940241989044798, 0.1140503700902940241989044798]`; per-observation stationarity residual `2.191626134359253893785762967e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp:219` (step stop): `inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {`. Execution-path causation remains open.

## Case 2414: ged, constant-1-1e-12

Data: `[0.999999999998, 0.999999999999, 1.0, 1.000000000001, 1.000000000003]`. Holds: `{'mean': -0.017698887677594307, 'sd': 1.0178559902092952}`.
Call: `route_ged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2414` (run through the slot wrapper).
Returned: `{'mean': '-0.017698887677594307', 'sd': '1.0178559902092952', 'nu': '7.1577709132690321'}`, status `0`, logLik `-6.190780779696186897353982203640043735504150390625`.
Independent logLik at that point: `-6.190780779696188025559970299` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.66e-08`.
Better feasible point: `[-0.01769888767759430700521328106, 1.01785599020929518054856544, 7.157771031871148318294126608]`; logLik `-6.190780779696187947277230401`.
Stationary local point: `[-0.01769888767759430700521328106, 1.01785599020929518054856544, 7.157771031871148318294126608]`; logLik `-6.190780779696187947277230401`. Verified coefficient relative error: `1.66e-08`.
Local information eigenvalue range: `[0.1140503722858558017499675353, 0.1140503722858558017499675353]`; per-observation stationarity residual `1.579723363670719210672133679e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp:219` (step stop): `inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {`. Execution-path causation remains open.

## Case 2464: ged, scale-1e+150

Data: `[7.321666961121761e+149, 2.276604294726238e+150, -5.491691631533211e+149, 1.5779966110538917e+150, -2.5578223779382837e+149, -2.1339634443158128e+148, -1.1263577338146473e+150, -8.880300857748881e+149, -8.586426179623096e+149, 3.2437851494901025e+149, 4.0891269493055628e+149, -4.340350619199568e+149]`. Holds: `{'sd': 1.0178559902092952, 'nu': 3.016645308327854}`.
Call: `route_ged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2464` (run through the slot wrapper).
Returned: `{'mean': '2.6626333795358893e+149', 'sd': '1.0178559902092952', 'nu': '3.0166453083278539'}`, status `0`, logLik `-inf`.
Independent logLik at that point: `-1.161320904786074869791283883e+453` (230 digits).
Evidence: status Ok with nonfinite reported logLik. Worst relative discrepancy: `inf`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp:219` (step stop): `inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {`. Execution-path causation remains open.

## Case 2480: ged, scale-1e+150

Data: `[7.321666961121761e+149, 2.276604294726238e+150, -5.491691631533211e+149, 1.5779966110538917e+150, -2.5578223779382837e+149, -2.1339634443158128e+148, -1.1263577338146473e+150, -8.880300857748881e+149, -8.586426179623096e+149, 3.2437851494901025e+149, 4.0891269493055628e+149, -4.340350619199568e+149]`. Holds: `{'mean': -0.017698887677594307, 'sd': 1.0178559902092952, 'nu': 3.016645308327854}`.
Call: `route_ged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2480` (run through the slot wrapper).
Returned: `{'mean': '-0.017698887677594307', 'sd': '1.0178559902092952', 'nu': '3.0166453083278539'}`, status `0`, logLik `-inf`.
Independent logLik at that point: `-1.303246957004904645178392327e+453` (230 digits).
Evidence: status Ok with nonfinite reported logLik. Worst relative discrepancy: `inf`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp:219` (step stop): `inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {`. Execution-path causation remains open.

## Case 2484: genbeta, interior

Data: `[0.9958942681116518, 0.9824191273831285, 0.2122736680461207, 0.9474954547057607, 0.9999970357577068, 0.8689219882446494, 0.9743349826257024, 0.9999994579613773, 0.9999987396939521, 0.6189318004593716, 0.9551767414779706, 0.924324002967906]`. Holds: `{'scale': 1.0}`.
Call: `route_genbeta(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2484` (run through the slot wrapper).
Returned: `{'shape1': '1.2376592542909757', 'shape2': '0.1938475240504321', 'shape3': '1.0654164157085864', 'scale': '1'}`, status `0`, logLik `33.493625473724961238985997624695301055908203125`.
Independent logLik at that point: `33.49362547372492156189424742` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.29e-07`.
Better feasible point: `[1.237659127007651834451683109, 0.1938475237402182167492847361, 1.065416553652191259810918797, 1.0]`; logLik `33.49362547372492175313647408`.
Stationary local point: `[1.237659127007651834451683109, 0.1938475237402182167492847361, 1.065416553652191259810918797, 1.0]`; logLik `33.49362547372492175313647408`. Verified coefficient relative error: `1.29e-07`.
Local information eigenvalue range: `[0.001163817361590725337712865219, 1.154909231545749750822768988]`; per-observation stationarity residual `1.113820701805162951661329322e-51`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/genbeta/mle.hpp:45` (special difference): `const double ga=fa?a*(-s+std::log1p(b/a)+trgamma_detail::log_minus_digamma(a)-u):0;`. Execution-path causation remains open.

## Case 2486: genbeta, tiny-3

Data: `[0.9958942681116518, 0.9824191273831285, 0.2122736680461207]`. Holds: `{'scale': 1.0}`.
Call: `route_genbeta(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2486` (run through the slot wrapper).
Returned: `{'shape1': '0.4250662078585058', 'shape2': '0.27056816239252646', 'shape3': '2.2066872967896973', 'scale': '1'}`, status `0`, logLik `3.28822049764943269423156380071304738521575927734375`.
Independent logLik at that point: `3.288220497649430460391737148` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.98e-08`.
Better feasible point: `[0.4250662144278413923091768371, 0.2705681628201192281723273152, 2.206687253085723217361340849, 1.0]`; logLik `3.288220497649430487577491519`.
Stationary local point: `[0.4250662144278413923091768371, 0.2705681628201192281723273152, 2.206687253085723217361340849, 1.0]`; logLik `3.288220497649430487577491519`. Verified coefficient relative error: `1.98e-08`.
Local information eigenvalue range: `[0.02802189000009210123654438315, 1.347067702862922984729248934]`; per-observation stationarity residual `1.565381868229845353317607695e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/genbeta/mle.hpp:45` (special difference): `const double ga=fa?a*(-s+std::log1p(b/a)+trgamma_detail::log_minus_digamma(a)-u):0;`. Execution-path causation remains open.

## Case 2487: genbeta, tiny-5

Data: `[0.9958942681116518, 0.9824191273831285, 0.2122736680461207, 0.9474954547057607, 0.9999970357577068]`. Holds: `{'scale': 1.0}`.
Call: `route_genbeta(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2487` (run through the slot wrapper).
Returned: `{'shape1': '0.6330263700701888', 'shape2': '0.18790664769863358', 'shape3': '1.5620378152991712', 'scale': '1'}`, status `0`, logLik `12.331901388570969402280752547085285186767578125`.
Independent logLik at that point: `12.33190138857096755527332037` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.33e-08`.
Better feasible point: `[0.6330263950014468593724054382, 0.1879066480831410805145521422, 1.562037731990648243039100278, 1.0]`; logLik `12.33190138857096767864988974`.
Stationary local point: `[0.6330263950014468593724054382, 0.1879066480831410805145521422, 1.562037731990648243039100278, 1.0]`; logLik `12.33190138857096767864988974`. Verified coefficient relative error: `5.33e-08`.
Local information eigenvalue range: `[0.01110085257018413275480518679, 1.16282257569801151567709502]`; per-observation stationarity residual `4.777928878080111987347220688e-57`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/genbeta/mle.hpp:45` (special difference): `const double ga=fa?a*(-s+std::log1p(b/a)+trgamma_detail::log_minus_digamma(a)-u):0;`. Execution-path causation remains open.

## Case 2492: genbeta, constant-0.25-0.0001

Data: `[0.24995, 0.249975, 0.25, 0.250025, 0.250075]`. Holds: `{'scale': 1.0}`.
Call: `route_genbeta(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2492` (run through the slot wrapper).
Returned: `{'shape1': '2648496744608.5767', 'shape2': '64929108.884116553', 'shape3': '1.7684202518402834e-05', 'scale': '1'}`, status `0`, logLik `43.1755971727898213430307805538177490234375`.
Independent logLik at that point: `43.17559621879210341443893316` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `2.21e-08`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/genbeta/mle.hpp:45` (special difference): `const double ga=fa?a*(-s+std::log1p(b/a)+trgamma_detail::log_minus_digamma(a)-u):0;`. Execution-path causation remains open.

## Case 2524: genbeta, tiny-3

Data: `[0.9958942681116518, 0.9824191273831285, 0.2122736680461207]`. Holds: `{'shape2': 0.24298710443400298, 'scale': 1.0}`.
Call: `route_genbeta(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2524` (run through the slot wrapper).
Returned: `{'shape1': '0.37175150324036277', 'shape2': '0.24298710443400298', 'shape3': '2.4672856715538223', 'scale': '1'}`, status `0`, logLik `3.277297007388424443519170381478033959865570068359375`.
Independent logLik at that point: `3.277297007388414928398815888` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.34e-08`.
Better feasible point: `[0.3717515068451528082699794057, 0.2429871044340029817565351777, 2.467285638559024134483053932, 1.0]`; logLik `3.277297007388414945222260613`.
Stationary local point: `[0.3717515068451528082699794057, 0.2429871044340029817565351777, 2.467285638559024134483053932, 1.0]`; logLik `3.277297007388414945222260613`. Verified coefficient relative error: `1.34e-08`.
Local information eigenvalue range: `[0.04021015576316847994960398436, 1.03367909266504109821205717]`; per-observation stationarity residual `7.36461508173171354818875809e-66`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/genbeta/mle.hpp:45` (special difference): `const double ga=fa?a*(-s+std::log1p(b/a)+trgamma_detail::log_minus_digamma(a)-u):0;`. Execution-path causation remains open.

## Case 2525: genbeta, tiny-5

Data: `[0.9958942681116518, 0.9824191273831285, 0.2122736680461207, 0.9474954547057607, 0.9999970357577068]`. Holds: `{'shape2': 0.24298710443400298, 'scale': 1.0}`.
Call: `route_genbeta(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2525` (run through the slot wrapper).
Returned: `{'shape1': '1.2015526618059686', 'shape2': '0.24298710443400298', 'shape3': '0.8316693227668247', 'scale': '1'}`, status `0`, logLik `12.1946382681867220298954634927213191986083984375`.
Independent logLik at that point: `12.19463826818671021428360563` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `8.42e-08`.
Better feasible point: `[1.201552742482118192446661764, 0.2429871044340029817565351777, 0.831669252781460289372468925, 1.0]`; logLik `12.19463826818671026926907729`.
Stationary local point: `[1.201552742482118192446661764, 0.2429871044340029817565351777, 0.831669252781460289372468925, 1.0]`; logLik `12.19463826818671026926907729`. Verified coefficient relative error: `8.42e-08`.
Local information eigenvalue range: `[0.001894321489840810802059544503, 0.667652143798427913633598752]`; per-observation stationarity residual `2.856791399771497532648230797e-53`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/genbeta/mle.hpp:45` (special difference): `const double ga=fa?a*(-s+std::log1p(b/a)+trgamma_detail::log_minus_digamma(a)-u):0;`. Execution-path causation remains open.

## Case 2667: gev, tiny-5

Data: `[-0.6514892575075315, 0.7190118994986324, 1.1956145927670203, 0.02135443799267926, 1.33255002155901]`. Holds: `{}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2667` (run through the slot wrapper).
Returned: `{'mu': '0.52340833886196214', 'sigma': '0.80914168269704778', 'xi': '-1'}`, status `0`, logLik `-3.9410937793300405`.
Independent logLik at that point: `-inf` (70 digits).
Evidence: returned coefficients exclude an observation from support. Worst relative discrepancy: `inf`.
Exact support exclusion: `{'bound': 'upper', 'observation_index': 4, 'exact_bound': '12002543561091407/9007199254740992', 'exact_observation': '750158972568213/562949953421312', 'exact_gap': '1/9007199254740992', 'bound_expression': 'Piecewise((p_mu - p_sigma/p_xi, p_xi < 0), (oo, True))', 'basis': 'exact binary64 rational support comparison'}`. This is a returned-coefficient feasibility defect; a one-ULP projection can be practically small even though the exact returned-point likelihood is zero.
Suspected cause / path: Boundary reconstruction/projection leaves the exact returned support short of the data; inspect `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 2682: gev, tiny-3

Data: `[-0.6514892575075315, 0.7190118994986324, 1.1956145927670203]`. Holds: `{'mu': 0.006623074223695902}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2682` (run through the slot wrapper).
Returned: `{'mu': '0.0066230742236959017', 'sigma': '1.1889915185433242', 'xi': '-1'}`, status `0`, logLik `-2.47366726845304`.
Independent logLik at that point: `-inf` (70 digits).
Evidence: returned coefficients exclude an observation from support. Worst relative discrepancy: `inf`.
Exact support exclusion: `{'bound': 'upper', 'observation_index': 2, 'exact_bound': '5384569434464279/4503599627370496', 'exact_observation': '673071179308035/562949953421312', 'exact_gap': '1/4503599627370496', 'bound_expression': 'Piecewise((p_mu - p_sigma/p_xi, p_xi < 0), (oo, True))', 'basis': 'exact binary64 rational support comparison'}`. This is a returned-coefficient feasibility defect; a one-ULP projection can be practically small even though the exact returned-point likelihood is zero.
Suspected cause / path: Boundary reconstruction/projection leaves the exact returned support short of the data; inspect `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 2694: gev, limit-outlier

Data: `[-0.6514892575075315, 0.7190118994986324, 1.1956145927670203, 0.02135443799267926, 1000001.7844905467]`. Holds: `{'mu': 0.006623074223695902}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2694` (run through the slot wrapper).
Returned: `{'mu': '0.0066230742236959017', 'sigma': '1.0139222806325212', 'xi': '1'}`, status `0`, logLik `-33.106111822889516815848764963448047637939453125`.
Independent logLik at that point: `-33.10611182288951672509156533` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.35e-08`.
Better feasible point: `[0.006623074223695901707742450526, 1.013922236478083474111536068, 1.0]`; logLik `-33.10611182288950231177020237`.
Stationary local point: `[0.006623074223695901707742450526, 1.013922236478083474111536068, 1.0]`; logLik `-33.10611182288950231177020237`. Verified coefficient relative error: `4.35e-08`.
Local information eigenvalue range: `[3.040078140716044551561479434, 3.040078140716044551561479434]`; per-observation stationarity residual `6.269003586044004655083472725e-54`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 2695: gev, limit-concentration

Data: `[0.7950005938035326, 0.7950013888057164, 0.7950021838079002, 0.795002978810084, 0.7950037738122678]`. Holds: `{'mu': 0.006623074223695902}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2695` (run through the slot wrapper).
Returned: `{'mu': '0.0066230742236959017', 'sigma': '0.78838069958857182', 'xi': '-1'}`, status `0`, logLik `1.1888608384340265`.
Independent logLik at that point: `-inf` (70 digits).
Evidence: returned coefficients exclude an observation from support. Worst relative discrepancy: `inf`.
Exact support exclusion: `{'bound': 'upper', 'observation_index': 4, 'exact_bound': '3580378699499067/4503599627370496', 'exact_observation': '7160757398998135/9007199254740992', 'exact_gap': '1/9007199254740992', 'bound_expression': 'Piecewise((p_mu - p_sigma/p_xi, p_xi < 0), (oo, True))', 'basis': 'exact binary64 rational support comparison'}`. This is a returned-coefficient feasibility defect; a one-ULP projection can be practically small even though the exact returned-point likelihood is zero.
Suspected cause / path: Boundary reconstruction/projection leaves the exact returned support short of the data; inspect `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 2696: gev, interior

Data: `[-0.6514892575075315, 0.7190118994986324, 1.1956145927670203, 0.02135443799267926, 1.33255002155901, 0.999055037892868, 0.4443082381781675, 1.784490546684275, 0.7950021838079002, 1.1415363726467913, -0.6653630472986568, -0.8686654093177045]`. Holds: `{'sigma': 0.9955800131732704}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2696` (run through the slot wrapper).
Returned: `{'mu': '0.33135743902177783', 'sigma': '0.99558001317327038', 'xi': '-0.63509189528641807'}`, status `0`, logLik `-14.0861365488525649425355368293821811676025390625`.
Independent logLik at that point: `-14.08613654885256505814262537` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.34e-08`.
Better feasible point: `[0.3313574524290498665091153562, 0.9955800131732703839304576832, -0.635091904145199870843446886]`; logLik `-14.08613654885256382692221761`.
Stationary local point: `[0.3313574524290498665091153562, 0.9955800131732703839304576832, -0.635091904145199870843446886]`; logLik `-14.08613654885256382692221761`. Verified coefficient relative error: `1.34e-08`.
Local information eigenvalue range: `[0.930247286119825063787446733, 36.6440230704070496391973404]`; per-observation stationarity residual `3.865596946036679248608687284e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 2707: gev, scale-1e-150

Data: `[-6.514892575075315e-151, 7.190118994986324e-151, 1.1956145927670203e-150, 2.1354437992679257e-152, 1.33255002155901e-150, 9.99055037892868e-151, 4.443082381781675e-151, 1.784490546684275e-150, 7.950021838079003e-151, 1.1415363726467913e-150, -6.653630472986568e-151, -8.686654093177045e-151]`. Holds: `{'sigma': 0.9955800131732704}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2707` (run through the slot wrapper).
Returned: `{'mu': '-0.99558001317327038', 'sigma': '0.99558001317327038', 'xi': '-1'}`, status `0`, logLik `0.053157406171572684`.
Independent logLik at that point: `-inf` (231 digits).
Evidence: returned coefficients exclude an observation from support. Worst relative discrepancy: `inf`.
Exact support exclusion: `{'bound': 'upper', 'observation_index': 1, 'exact_bound': '0', 'exact_observation': '2649925675495509/3685510180489786476798393145496356338786055879312930105836138965083617346086082863365358130056307390177215209990980317284932211552660930305235775636164742230126362624', 'exact_gap': '2649925675495509/3685510180489786476798393145496356338786055879312930105836138965083617346086082863365358130056307390177215209990980317284932211552660930305235775636164742230126362624', 'bound_expression': 'Piecewise((p_mu - p_sigma/p_xi, p_xi < 0), (oo, True))', 'basis': 'exact binary64 rational support comparison'}`. This is a returned-coefficient feasibility defect; a one-ULP projection can be practically small even though the exact returned-point likelihood is zero.
Suspected cause / path: Boundary reconstruction/projection leaves the exact returned support short of the data; inspect `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 2709: gev, offset-1e8

Data: `[99999999.34851074, 100000000.7190119, 100000001.19561459, 100000000.02135444, 100000001.33255002, 100000000.99905504, 100000000.44430824, 100000001.78449054, 100000000.79500218, 100000001.14153637, 99999999.33463696, 99999999.13133459]`. Holds: `{'sigma': 0.9955800131732704}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2709` (run through the slot wrapper).
Returned: `{'mu': '100000000.33135743', 'sigma': '0.99558001317327038', 'xi': '-0.63509189140568667'}`, status `0`, logLik `-14.086136519801424782372123445384204387664794921875`.
Independent logLik at that point: `-14.086136519801424127327367` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.65e-08`.
Better feasible point: `[100000000.3313574527658360677, 0.9955800131732703839304576832, -0.6350919079293311847301600347]`; logLik `-14.08613651980141909304282061`.
Stationary local point: `[100000000.3313574527658360677, 0.9955800131732703839304576832, -0.6350919079293311847301600347]`; logLik `-14.08613651980141909304282061`. Verified coefficient relative error: `1.65e-08`.
Local information eigenvalue range: `[0.930247269672343048915894801, 36.64402394691099042445705244]`; per-observation stationarity residual `1.568723014236777555771540577e-55`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 2710: gev, limit-outlier

Data: `[-0.6514892575075315, 0.7190118994986324, 1.1956145927670203, 0.02135443799267926, 1000001.7844905467]`. Holds: `{'sigma': 0.9955800131732704}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2710` (run through the slot wrapper).
Returned: `{'mu': '-0.0018995456336381444', 'sigma': '0.99558001317327038', 'xi': '1'}`, status `0`, logLik `-33.10762221832652585362666286528110504150390625`.
Independent logLik at that point: `-33.10762221832652627197566506` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.47e-08`.
Better feasible point: `[-0.001899520886237193607853113486, 0.9955800131732703839304576832, 1.0]`; logLik `-33.10762221832651683706688389`.
Stationary local point: `[-0.001899520886237193607853113486, 0.9955800131732703839304576832, 1.0]`; logLik `-33.10762221832651683706688389`. Verified coefficient relative error: `2.47e-08`.
Local information eigenvalue range: `[6162269118034.821737742145236, 6162269118034.821737742145236]`; per-observation stationarity residual `9.182991502328403561567297726e-50`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 2749: gev, constant-0.795002-0.0001

Data: `[0.7948431833711387, 0.7949226835895195, 0.7950021838079002, 0.795081684026281, 0.7952406844630426]`. Holds: `{'mu': 0.006623074223695902, 'xi': 0.30843601546572924}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2749` (run through the slot wrapper).
Returned: `{'mu': '0.0066230742236959017', 'sigma': '0.54848922454540783', 'xi': '0.30843601546572924'}`, status `0`, logLik `-6.3021219003578199391313319210894405841827392578125`.
Independent logLik at that point: `-6.302121900357820815816669421` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.43e-08`.
Better feasible point: `[0.006623074223695901707742450526, 0.5484892167230755216572683916, 0.3084360154657292407698321313]`; logLik `-6.302121900357820310065267814`.
Stationary local point: `[0.006623074223695901707742450526, 0.5484892167230755216572683916, 0.3084360154657292407698321313]`; logLik `-6.302121900357820310065267814`. Verified coefficient relative error: `1.43e-08`.
Local information eigenvalue range: `[0.9946282052095056126800649387, 0.9946282052095056126800649387]`; per-observation stationarity residual `1.036336481131304525804154465e-61`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 2750: gev, constant-0.795002-1e-08

Data: `[0.7950021679078565, 0.7950021758578784, 0.7950021838079002, 0.7950021917579221, 0.7950022076579657]`. Holds: `{'mu': 0.006623074223695902, 'xi': 0.30843601546572924}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2750` (run through the slot wrapper).
Returned: `{'mu': '0.0066230742236959017', 'sigma': '0.5484781704640399', 'xi': '0.30843601546572924'}`, status `0`, logLik `-6.30202107177608095156529088853858411312103271484375`.
Independent logLik at that point: `-6.30202107177608099606161266` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.01e-08`.
Better feasible point: `[0.006623074223695901707742450526, 0.5484781594374708228954767177, 0.3084360154657292407698321313]`; logLik `-6.30202107177607999106895333`.
Stationary local point: `[0.006623074223695901707742450526, 0.5484781594374708228954767177, 0.3084360154657292407698321313]`; logLik `-6.30202107177607999106895333`. Verified coefficient relative error: `2.01e-08`.
Local information eigenvalue range: `[0.9946282279585251545455203469, 0.9946282279585251545455203469]`; per-observation stationarity residual `1.615860963080875012510230254e-60`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 2757: gev, offset-1e8

Data: `[99999999.34851074, 100000000.7190119, 100000001.19561459, 100000000.02135444, 100000001.33255002, 100000000.99905504, 100000000.44430824, 100000001.78449054, 100000000.79500218, 100000001.14153637, 99999999.33463696, 99999999.13133459]`. Holds: `{'mu': 0.006623074223695902, 'xi': 0.30843601546572924}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2757` (run through the slot wrapper).
Returned: `{'mu': '0.0066230742236959017', 'sigma': '69570358.078486726', 'xi': '0.30843601546572924'}`, status `0`, logLik `-239.026333952579079777933657169342041015625`.
Independent logLik at that point: `-239.026333952579082745896881` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.53e-08`.
Better feasible point: `[0.006623074223695901707742450526, 69570357.0125944663966760199, 0.3084360154657292407698321313]`; logLik `-239.0263339525790813450516434`.
Stationary local point: `[0.006623074223695901707742450526, 69570357.0125944663966760199, 0.3084360154657292407698321313]`; logLik `-239.0263339525790813450516434`. Verified coefficient relative error: `1.53e-08`.
Local information eigenvalue range: `[0.9946282184441796020694585758, 0.9946282184441796020694585758]`; per-observation stationarity residual `1.838518787964629384716824761e-61`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 2758: gev, limit-outlier

Data: `[-0.6514892575075315, 0.7190118994986324, 1.1956145927670203, 0.02135443799267926, 1000001.7844905467]`. Holds: `{'mu': 0.006623074223695902, 'xi': 0.30843601546572924}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2758` (run through the slot wrapper).
Returned: `{'mu': '0.0066230742236959017', 'sigma': '1.7227198241791626', 'xi': '0.30843601546572924'}`, status `0`, logLik `-58.542844299081451708843815140426158905029296875`.
Independent logLik at that point: `-58.54284429908144992897195905` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.61e-08`.
Better feasible point: `[0.006623074223695901707742450526, 1.722719779292538630307460936, 0.3084360154657292407698321313]`; logLik `-58.54284429908144951769644998`.
Stationary local point: `[0.006623074223695901707742450526, 1.722719779292538630307460936, 0.3084360154657292407698321313]`; logLik `-58.54284429908144951769644998`. Verified coefficient relative error: `2.61e-08`.
Local information eigenvalue range: `[0.2423194509815893982898212656, 0.2423194509815893982898212656]`; per-observation stationarity residual `2.235384181613580088975744563e-59`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 2761: gev, tiny-2

Data: `[-0.6514892575075315, 0.7190118994986324]`. Holds: `{'sigma': 0.9955800131732704, 'xi': 0.30843601546572924}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2761` (run through the slot wrapper).
Returned: `{'mu': '-0.092075816968854807', 'sigma': '0.99558001317327038', 'xi': '0.30843601546572924'}`, status `0`, logLik `-2.4716416526012974230752661242149770259857177734375`.
Independent logLik at that point: `-2.47164165260129744390180645` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.17e-08`.
Better feasible point: `[-0.09207580524987673825806176537, 0.9955800131732703839304576832, 0.3084360154657292407698321313]`; logLik `-2.471641652601297228839620858`.
Stationary local point: `[-0.09207580524987673825806176537, 0.9955800131732703839304576832, 0.3084360154657292407698321313]`; logLik `-2.471641652601297228839620858`. Verified coefficient relative error: `1.17e-08`.
Local information eigenvalue range: `[2.941327537056807916565473761, 2.941327537056807916565473761]`; per-observation stationarity residual `1.640269933986836099845276348e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 2762: gev, tiny-3

Data: `[-0.6514892575075315, 0.7190118994986324, 1.1956145927670203]`. Holds: `{'sigma': 0.9955800131732704, 'xi': 0.30843601546572924}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2762` (run through the slot wrapper).
Returned: `{'mu': '0.087035747855161327', 'sigma': '0.99558001317327038', 'xi': '0.30843601546572924'}`, status `0`, logLik `-4.16133786253508475283524603582918643951416015625`.
Independent logLik at that point: `-4.161337862535085401446093758` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.19e-08`.
Better feasible point: `[0.08703575971890196957004061603, 0.9955800131732703839304576832, 0.3084360154657292407698321313]`; logLik `-4.161337862535085066728698659`.
Stationary local point: `[0.08703575971890196957004061603, 0.9955800131732703839304576832, 0.3084360154657292407698321313]`; logLik `-4.161337862535085066728698659`. Verified coefficient relative error: `1.19e-08`.
Local information eigenvalue range: `[5.409116901134575247562439379, 5.409116901134575247562439379]`; per-observation stationarity residual `3.368941049138592576076610642e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 2763: gev, tiny-5

Data: `[-0.6514892575075315, 0.7190118994986324, 1.1956145927670203, 0.02135443799267926, 1.33255002155901]`. Holds: `{'sigma': 0.9955800131732704, 'xi': 0.30843601546572924}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2763` (run through the slot wrapper).
Returned: `{'mu': '0.22428142653904304', 'sigma': '0.99558001317327038', 'xi': '0.30843601546572924'}`, status `0`, logLik `-6.79756092810537548842830801731906831264495849609375`.
Independent logLik at that point: `-6.797560928105375394022312928` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.1e-08`.
Better feasible point: `[0.2242814375301880627459781551, 0.9955800131732703839304576832, 0.3084360154657292407698321313]`; logLik `-6.797560928105374909081623128`.
Stationary local point: `[0.2242814375301880627459781551, 0.9955800131732703839304576832, 0.3084360154657292407698321313]`; logLik `-6.797560928105374909081623128`. Verified coefficient relative error: `1.1e-08`.
Local information eigenvalue range: `[6.320672322724879657013801393, 6.320672322724879657013801393]`; per-observation stationarity residual `2.179944064733697039926486476e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 2793: gompertz, tiny-2

Data: `[0.9923294273400786, 0.4321380236025882]`. Holds: `{}`.
Call: `route_gompertz(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 2793` (run through the slot wrapper).
Returned: `{'a': '0.15831685073294502', 'b': '3.8422900821528634'}`, status `0`, logLik `-0.213096580230389687216074889875017106533050537109375`.
Independent logLik at that point: `-0.2130965802303896567542705665` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.63e-08`.
Better feasible point: `[0.1583168465637951845986170713, 3.842290119126981965978862586]`; logLik `-0.2130965802303895736567121368`.
Stationary local point: `[0.1583168465637951845986170713, 3.842290119126981965978862586]`; logLik `-0.2130965802303895736567121368`. Verified coefficient relative error: `2.63e-08`.
Local information eigenvalue range: `[0.09659763188600798062699348342, 9.289801739976084238320514431]`; per-observation stationarity residual `3.794499126494476667617907179e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gompertz/mle.hpp:98` (step stop): `if (std::abs(step) < (std::abs(b) * tol + tol)) break;`. Execution-path causation remains open.

## Case 3008: gumbel, scale-1e+150

Data: `[2.322822415496901e+150, -8.046749604975467e+148, -1.110891503229487e+150, 3.408709691847847e+150, 3.8590223814265865e+150, 5.021748072032642e+150, 2.7853634656396305e+150, -2.544522928870159e+150, 2.465816834417481e+150, 7.536400107373606e+149, 4.537716580414708e+150, 9.719677186278473e+150]`. Holds: `{'mu': 2.964923020698754, 'sigma': 3.021078660513882}`.
Call: `route_gumbel(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3008` (run through the slot wrapper).
Returned: `{'mu': '2.964923020698754', 'sigma': '3.021078660513882'}`, status `0`, logLik `-inf`.
Independent logLik at that point: `-2.291226038735361564673866874e+365787320114544478389227278750615591212480999417151785526986378127751405837288381994762589088888348738922971444444941791149488775068519542322108688027` (230 digits).
Evidence: status Ok with nonfinite reported logLik. Worst relative discrepancy: `inf`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gumbel/mle.hpp:96` (step stop): `if (std::abs(dth) < std::abs(theta) * tol) break;`. Execution-path causation remains open.

## Case 3166: ht, interior

Data: `[1.127474126772717, 0.8953762548544653, 0.35429257093552485, 2.282929373212121, 18.791453452792773, 0.11217703274133166, 4.372828590994294, 1.576797187376036, 1.960176001598786, 0.9790564081851776, 0.9688548171788223, 1.6937507700593049]`. Holds: `{}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3166` (run through the slot wrapper).
Returned: `{'mu': '0.11217703274133166', 'nu': '1.3654144923660534', 'sigma': '1.3388645269319408'}`, status `0`, logLik `-21.880455057529818674311172799207270145416259765625`.
Independent logLik at that point: `-21.88045505752982481946087514` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.02e-08`.
Better feasible point: `[0.1121770327413316586939728836, 1.365414478474467823825233015, 1.338864522828923202786071137]`; logLik `-21.88045505752982462737045259`.
Stationary local point: `[0.1121770327413316586939728836, 1.365414478474467823825233015, 1.338864522828923202786071137]`; logLik `-21.88045505752982462737045259`. Verified coefficient relative error: `1.02e-08`.
Local information eigenvalue range: `[0.2700965253239167359502813097, 0.8250273534229353618245586476]`; per-observation stationarity residual `1.065324859106816581747380776e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3177: ht, lower-1e-06

Data: `[1e-06, 2e-06, 3e-06, 4.9999999999999996e-06, 9e-06]`. Holds: `{}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3177` (run through the slot wrapper).
Returned: `{'mu': '9.9999999999999995e-07', 'nu': '14.687091266323772', 'sigma': '3.8466045998849529e-06'}`, status `0`, logLik `58.366686218476417025158298201858997344970703125`.
Independent logLik at that point: `58.36668621847640904829276308` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.98e-06`.
Better feasible point: `[0.0000009999999999999999547481118259, 14.68706220990046048420288788, 0.000003846604069799454980206390248]`; logLik `58.36668621847641438235554083`.
Stationary local point: `[0.0000009999999999999999547481118259, 14.68706220990046048420288788, 0.000003846604069799454980206390248]`; logLik `58.36668621847641438235554083`. Verified coefficient relative error: `1.98e-06`.
Local information eigenvalue range: `[0.0005425058708625515783938344473, 1.651517464712926706241358134]`; per-observation stationarity residual `3.884149952876503179883733052e-43`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3178: ht, lower-1e-12

Data: `[1e-12, 2e-12, 3e-12, 5e-12, 9e-12]`. Holds: `{}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3178` (run through the slot wrapper).
Returned: `{'mu': '9.9999999999999998e-13', 'nu': '14.68712334874507', 'sigma': '3.8466051851713254e-12'}`, status `0`, logLik `127.444239008297785176182514987885951995849609375`.
Independent logLik at that point: `127.4442390082977615334527868` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.16e-06`.
Better feasible point: `[9.999999999999999798866476293e-13, 14.68706220990057386977925256, 3.846604069799456876352535614e-12]`; logLik `127.4442390082977851494456651`.
Stationary local point: `[9.999999999999999798866476293e-13, 14.68706220990057386977925256, 3.846604069799456876352535614e-12]`; logLik `127.4442390082977851494456651`. Verified coefficient relative error: `4.16e-06`.
Local information eigenvalue range: `[0.0005425082303060933056304313354, 1.651517999725350664427900843]`; per-observation stationarity residual `1.161181163243655653725179216e-70`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3179: ht, lower-1e-17

Data: `[1e-17, 2e-17, 3e-17, 5.0000000000000005e-17, 9.000000000000001e-17]`. Holds: `{}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3179` (run through the slot wrapper).
Returned: `{'mu': '1.0000000000000001e-17', 'nu': '14.687044500656688', 'sigma': '3.8466037467237316e-17'}`, status `0`, logLik `185.0088663331489442498423159122467041015625`.
Independent logLik at that point: `185.0088663331489246060921545` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.21e-06`.
Better feasible point: `[1.000000000000000071542424055e-17, 14.68706220990054065961045577, 3.846604069799456775007883396e-17]`; logLik `185.0088663331489265875135657`.
Stationary local point: `[1.000000000000000071542424055e-17, 14.68706220990054065961045577, 3.846604069799456775007883396e-17]`; logLik `185.0088663331489265875135657`. Verified coefficient relative error: `1.21e-06`.
Local information eigenvalue range: `[0.0005425024315753307512136931582, 1.651516684835708444538687276]`; per-observation stationarity residual `7.395361828259006375212290632e-45`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3180: ht, lower-1e-200

Data: `[1e-200, 2e-200, 3e-200, 5e-200, 9e-200]`. Holds: `{}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3180` (run through the slot wrapper).
Returned: `{'mu': '9.9999999999999998e-201', 'nu': '14.687136817648865', 'sigma': '3.846605430883535e-200'}`, status `0`, logLik `2291.8742264227012128685601055622100830078125`.
Independent logLik at that point: `2291.874226422700692573420617` (280 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.08e-06`.
Better feasible point: `[9.999999999999999821002623991e-201, 14.68706220990047674269273246, 3.846604069799455386313792928e-200]`; logLik `2291.874226422700727740698982`.
Stationary local point: `[9.999999999999999821002623991e-201, 14.68706220990047674269273246, 3.846604069799455386313792928e-200]`; logLik `2291.874226422700727740698982`. Verified coefficient relative error: `5.08e-06`.
Local information eigenvalue range: `[0.0005425092208537458921625690498, 1.651518224332057838692668486]`; per-observation stationarity residual `1.49027077568765950249588583e-76`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3181: ht, scale-1e-150

Data: `[1.1274741267727171e-150, 8.953762548544653e-151, 3.542925709355249e-151, 2.282929373212121e-150, 1.8791453452792772e-149, 1.1217703274133166e-151, 4.3728285909942937e-150, 1.576797187376036e-150, 1.9601760015987858e-150, 9.790564081851776e-151, 9.688548171788223e-151, 1.693750770059305e-150]`. Holds: `{}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3181` (run through the slot wrapper).
Returned: `{'mu': '1.1217703274133166e-151', 'nu': '1.3654151266826571', 'sigma': '1.3388647142844398e-150'}`, status `0`, logLik `4122.7727123317536097601987421512603759765625`.
Independent logLik at that point: `4122.77271233175198835273675` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.75e-07`.
Better feasible point: `[1.121770327413316611313566287e-151, 1.365414478474467855124875044, 1.338864522828923229273862733e-150]`; logLik `4122.772712331752406597860532`.
Stationary local point: `[1.121770327413316611313566287e-151, 1.365414478474467855124875044, 1.338864522828923229273862733e-150]`; logLik `4122.772712331752406597860532`. Verified coefficient relative error: `4.75e-07`.
Local information eigenvalue range: `[0.2700967432308841756117642245, 0.8250276852596464750504442302]`; per-observation stationarity residual `2.394328991345788147578249822e-50`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3182: ht, scale-1e+150

Data: `[1.127474126772717e+150, 8.953762548544653e+149, 3.5429257093552483e+149, 2.282929373212121e+150, 1.8791453452792773e+151, 1.1217703274133165e+149, 4.3728285909942935e+150, 1.576797187376036e+150, 1.9601760015987857e+150, 9.790564081851775e+149, 9.688548171788222e+149, 1.6937507700593047e+150]`. Holds: `{}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3182` (run through the slot wrapper).
Returned: `{'mu': '1.1217703274133165e+149', 'nu': '1.3654145787505616', 'sigma': '1.3388645524453775e+150'}`, status `0`, logLik `-4166.533622446810113615356385707855224609375`.
Independent logLik at that point: `-4166.533622446812065492585965` (231 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `7.34e-08`.
Better feasible point: `[1.121770327413316504582314941e+149, 1.365414478474467803245413949, 1.338864522828923143909051285e+150]`; logLik `-4166.533622446812055483443709`.
Stationary local point: `[1.121770327413316504582314941e+149, 1.365414478474467803245413949, 1.338864522828923143909051285e+150]`; logLik `-4166.533622446812055483443709`. Verified coefficient relative error: `7.34e-08`.
Local information eigenvalue range: `[0.2700965549995186314605483382, 0.8250273986129689219593413648]`; per-observation stationarity residual `7.853304629384487368935337446e-57`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3183: ht, offset-1e8

Data: `[100000001.12747413, 100000000.89537625, 100000000.35429257, 100000002.28292938, 100000018.79145345, 100000000.11217703, 100000004.37282859, 100000001.57679719, 100000001.960176, 100000000.9790564, 100000000.96885481, 100000001.69375077]`. Holds: `{}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3183` (run through the slot wrapper).
Returned: `{'mu': '100000000.11217703', 'nu': '1.3654145088389911', 'sigma': '1.3388645346222332'}`, status `0`, logLik `-21.880455076377312906288352678529918193817138671875`.
Independent logLik at that point: `-21.88045507637731995205401243` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.12e-08`.
Better feasible point: `[100000000.1121770292520523071, 1.365414479848675876148437791, 1.338864526059678348635437086]`; logLik `-21.88045507637731911547311621`.
Stationary local point: `[100000000.1121770292520523071, 1.365414479848675876148437791, 1.338864526059678348635437086]`; logLik `-21.88045507637731911547311621`. Verified coefficient relative error: `2.12e-08`.
Local information eigenvalue range: `[0.2700965301271963396741406617, 0.8250273615213814517150300559]`; per-observation stationarity residual `3.83258203423098895370103597e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3186: ht, interior

Data: `[1.127474126772717, 0.8953762548544653, 0.35429257093552485, 2.282929373212121, 18.791453452792773, 0.11217703274133166, 4.372828590994294, 1.576797187376036, 1.960176001598786, 0.9790564081851776, 0.9688548171788223, 1.6937507700593049]`. Holds: `{'mu': 0.0}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3186` (run through the slot wrapper).
Returned: `{'mu': '0', 'nu': '1.4491147886724591', 'sigma': '1.4886802480504699'}`, status `0`, logLik `-22.57341458449710813738420256413519382476806640625`.
Independent logLik at that point: `-22.5734145844971142938169814` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.02e-08`.
Better feasible point: `[0.0, 1.449114773944282656600849202, 1.488680243863691443925960531]`; logLik `-22.57341458449711410638441027`.
Stationary local point: `[0.0, 1.449114773944282656600849202, 1.488680243863691443925960531]`; logLik `-22.57341458449711410638441027`. Verified coefficient relative error: `1.02e-08`.
Local information eigenvalue range: `[0.2703287538531141476506718777, 0.8522006523574215023029048145]`; per-observation stationarity residual `1.096846772050033305552171864e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3189: ht, tiny-5

Data: `[1.127474126772717, 0.8953762548544653, 0.35429257093552485, 2.282929373212121, 18.791453452792773]`. Holds: `{'mu': 0.0}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3189` (run through the slot wrapper).
Returned: `{'mu': '0', 'nu': '0.87466418190068318', 'sigma': '1.3000004854737282'}`, status `0`, logLik `-11.3210517439219255919624629314057528972625732421875`.
Independent logLik at that point: `-11.32105174392192904057683217` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.88e-08`.
Better feasible point: `[0.0, 0.8746641983480150410042246558, 1.300000494230390924506567232]`; logLik `-11.32105174392192864915456804`.
Stationary local point: `[0.0, 0.8746641983480150410042246558, 1.300000494230390924506567232]`; logLik `-11.32105174392192864915456804`. Verified coefficient relative error: `1.88e-08`.
Local information eigenvalue range: `[0.3327378920131151225021020909, 0.7234657623597500198525478265]`; per-observation stationarity residual `1.164782303038330857516426656e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3201: ht, scale-1e-150

Data: `[1.1274741267727171e-150, 8.953762548544653e-151, 3.542925709355249e-151, 2.282929373212121e-150, 1.8791453452792772e-149, 1.1217703274133166e-151, 4.3728285909942937e-150, 1.576797187376036e-150, 1.9601760015987858e-150, 9.790564081851776e-151, 9.688548171788223e-151, 1.693750770059305e-150]`. Holds: `{'mu': 0.0}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3201` (run through the slot wrapper).
Returned: `{'mu': '0', 'nu': '1.4491141998849366', 'sigma': '1.4886800806744364e-150'}`, status `0`, logLik `4122.0797528047869491274468600749969482421875`.
Independent logLik at that point: `4122.079752804784832362535586` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.96e-07`.
Better feasible point: `[0.0, 1.449114773944282688733782612, 1.48868024386369147183447827e-150]`; logLik `4122.079752804785117110664446`.
Stationary local point: `[0.0, 1.449114773944282688733782612, 1.48868024386369147183447827e-150]`; logLik `4122.079752804785117110664446`. Verified coefficient relative error: `3.96e-07`.
Local information eigenvalue range: `[0.2703285588682638080977979903, 0.8522003828968549259288198587]`; per-observation stationarity residual `5.842607398641052084898988718e-51`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3202: ht, scale-1e+150

Data: `[1.127474126772717e+150, 8.953762548544653e+149, 3.5429257093552483e+149, 2.282929373212121e+150, 1.8791453452792773e+151, 1.1217703274133165e+149, 4.3728285909942935e+150, 1.576797187376036e+150, 1.9601760015987857e+150, 9.790564081851775e+149, 9.688548171788222e+149, 1.6937507700593047e+150]`. Holds: `{'mu': 0.0}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3202` (run through the slot wrapper).
Returned: `{'mu': '0', 'nu': '1.4491145138783985', 'sigma': '1.4886801699323211e+150'}`, status `0`, logLik `-4167.2265819737776837428100407123565673828125`.
Independent logLik at that point: `-4167.226581973779403385843446` (231 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.79e-07`.
Better feasible point: `[0.0, 1.449114773944282628268954861, 1.488680243863691372188496077e+150]`; logLik `-4167.226581973779344945338163`.
Stationary local point: `[0.0, 1.449114773944282628268954861, 1.488680243863691372188496077e+150]`; logLik `-4167.226581973779344945338163`. Verified coefficient relative error: `1.79e-07`.
Local information eigenvalue range: `[0.2703286628512857715156539191, 0.8522005265952674426181245328]`; per-observation stationarity residual `1.036613238134512559179817044e-53`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3204: ht, limit-outlier

Data: `[1.127474126772717, 0.8953762548544653, 0.35429257093552485, 2.282929373212121, 1576815.9788294889]`. Holds: `{'mu': 0.0}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3204` (run through the slot wrapper).
Returned: `{'mu': '0', 'nu': '0.23078560350038405', 'sigma': '0.88947440088914098'}`, status `0`, logLik `-27.11605374228287956839267280884087085723876953125`.
Independent logLik at that point: `-27.11605374228288140158075033` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.47e-08`.
Better feasible point: `[0.0, 0.2307856001088809544383680157, 0.8894743981198101446191393091]`; logLik `-27.11605374228288094873000871`.
Stationary local point: `[0.0, 0.2307856001088809544383680157, 0.8894743981198101446191393091]`; logLik `-27.11605374228288094873000871`. Verified coefficient relative error: `1.47e-08`.
Local information eigenvalue range: `[0.2581511937679129184095466099, 0.8558602920520178728951479114]`; per-observation stationarity residual `3.699177077980773239875830662e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3246: ht, interior

Data: `[1.127474126772717, 0.8953762548544653, 0.35429257093552485, 2.282929373212121, 18.791453452792773, 0.11217703274133166, 4.372828590994294, 1.576797187376036, 1.960176001598786, 0.9790564081851776, 0.9688548171788223, 1.6937507700593049]`. Holds: `{'sigma': 0.978535583741794}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3246` (run through the slot wrapper).
Returned: `{'mu': '0.11217703274133166', 'nu': '1.1213715304325425', 'sigma': '0.97853558374179395'}`, status `0`, logLik `-22.210133897804727354241549619473516941070556640625`.
Independent logLik at that point: `-22.21013389780473568055684251` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.14e-08`.
Better feasible point: `[0.1121770327413316586939728836, 1.121371543255897097995530012, 0.9785355837417939461886362551]`; logLik `-22.21013389780473534089772365`.
Stationary local point: `[0.1121770327413316586939728836, 1.121371543255897097995530012, 0.9785355837417939461886362551]`; logLik `-22.21013389780473534089772365`. Verified coefficient relative error: `1.14e-08`.
Local information eigenvalue range: `[0.4329001822572066643747364797, 0.4329001822572066643747364797]`; per-observation stationarity residual `1.430291384139910103174693189e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3263: ht, offset-1e8

Data: `[100000001.12747413, 100000000.89537625, 100000000.35429257, 100000002.28292938, 100000018.79145345, 100000000.11217703, 100000004.37282859, 100000001.57679719, 100000001.960176, 100000000.9790564, 100000000.96885481, 100000001.69375077]`. Holds: `{'sigma': 0.978535583741794}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3263` (run through the slot wrapper).
Returned: `{'mu': '100000000.11217703', 'nu': '1.1213715147024657', 'sigma': '0.97853558374179395'}`, status `0`, logLik `-22.21013392147535370213518035598099231719970703125`.
Independent logLik at that point: `-22.21013392147536194850309858` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.46e-08`.
Better feasible point: `[100000000.1121770292520523071, 1.12137154229482134889602147, 0.9785355837417939461886362551]`; logLik `-22.21013392147536037590575418`.
Stationary local point: `[100000000.1121770292520523071, 1.12137154229482134889602147, 0.9785355837417939461886362551]`; logLik `-22.21013392147536037590575418`. Verified coefficient relative error: `2.46e-08`.
Local information eigenvalue range: `[0.4329001710667210943673353701, 0.4329001710667210943673353701]`; per-observation stationarity residual `6.572362647498720449877037297e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3266: ht, interior

Data: `[1.127474126772717, 0.8953762548544653, 0.35429257093552485, 2.282929373212121, 18.791453452792773, 0.11217703274133166, 4.372828590994294, 1.576797187376036, 1.960176001598786, 0.9790564081851776, 0.9688548171788223, 1.6937507700593049]`. Holds: `{'mu': 0.0, 'sigma': 0.978535583741794}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3266` (run through the slot wrapper).
Returned: `{'mu': '0', 'nu': '1.1006997236571705', 'sigma': '0.97853558374179395'}`, status `0`, logLik `-23.19689393170771296581733622588217258453369140625`.
Independent logLik at that point: `-23.19689393170772163038820485` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.76e-08`.
Better feasible point: `[0.0, 1.100699693306485686118161525, 0.9785355837417939461886362551]`; logLik `-23.19689393170771961921782093`.
Stationary local point: `[0.0, 1.100699693306485686118161525, 0.9785355837417939461886362551]`; logLik `-23.19689393170771961921782093`. Verified coefficient relative error: `2.76e-08`.
Local information eigenvalue range: `[0.440857619653804275372412596, 0.440857619653804275372412596]`; per-observation stationarity residual `1.672760775572580936484571784e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3271: ht, constant-1.5768-0.0001

Data: `[1.5764818279385608, 1.5766395076572983, 1.576797187376036, 1.5769548670947737, 1.577270226532249]`. Holds: `{'mu': 0.0, 'sigma': 0.978535583741794}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3271` (run through the slot wrapper).
Returned: `{'mu': '0', 'nu': '14.412438143726687', 'sigma': '0.97853558374179395'}`, status `0`, logLik `-7.49007497404189592771217576228082180023193359375`.
Independent logLik at that point: `-7.490074974041899768476444684` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.88e-07`.
Better feasible point: `[0.0, 14.41243543475385979321564999, 0.9785355837417939461886362551]`; logLik `-7.490074974041899098173698709`.
Stationary local point: `[0.0, 14.41243543475385979321564999, 0.9785355837417939461886362551]`; logLik `-7.490074974041899098173698709`. Verified coefficient relative error: `1.88e-07`.
Local information eigenvalue range: `[0.007589213256710717166982544198, 0.007589213256710717166982544198]`; per-observation stationarity residual `1.54894057475412407873117702e-53`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3272: ht, constant-1.5768-1e-08

Data: `[1.5767971558400922, 1.5767971716080642, 1.576797187376036, 1.5767972031440078, 1.5767972346799517]`. Holds: `{'mu': 0.0, 'sigma': 0.978535583741794}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3272` (run through the slot wrapper).
Returned: `{'mu': '0', 'nu': '14.420304139922104', 'sigma': '0.97853558374179395'}`, status `0`, logLik `-7.4898395860832938097928490606136620044708251953125`.
Independent logLik at that point: `-7.489839586083298221501673276` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.03e-08`.
Better feasible point: `[0.0, 14.42030472079167634974903807, 0.9785355837417939461886362551]`; logLik `-7.489839586083298190747871229`.
Stationary local point: `[0.0, 14.42030472079167634974903807, 0.9785355837417939461886362551]`; logLik `-7.489839586083298190747871229`. Verified coefficient relative error: `4.03e-08`.
Local information eigenvalue range: `[0.007581416547555996613495916425, 0.007581416547555996613495916425]`; per-observation stationarity residual `6.886526737816502347861483792e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3273: ht, constant-1.5768-1e-12

Data: `[1.5767971873728823, 1.5767971873744593, 1.576797187376036, 1.5767971873776128, 1.5767971873807665]`. Holds: `{'mu': 0.0, 'sigma': 0.978535583741794}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3273` (run through the slot wrapper).
Returned: `{'mu': '0', 'nu': '14.420309257814994', 'sigma': '0.97853558374179395'}`, status `0`, logLik `-7.4898395625561935418090797611512243747711181640625`.
Independent logLik at that point: `-7.489839562556198368696917687` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.6e-07`.
Better feasible point: `[0.0, 14.42030550604512802908379344, 0.9785355837417939461886362551]`; logLik `-7.489839562556197085735468149`.
Stationary local point: `[0.0, 14.42030550604512802908379344, 0.9785355837417939461886362551]`; logLik `-7.489839562556197085735468149`. Verified coefficient relative error: `2.6e-07`.
Local information eigenvalue range: `[0.007581420326381936354809752058, 0.007581420326381936354809752058]`; per-observation stationarity residual `2.085746967002331114330724598e-52`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3274: ht, constant-1.5768-0.0001

Data: `[1.5764818279385608, 1.5766395076572983, 1.576797187376036, 1.5769548670947737, 1.577270226532249]`. Holds: `{'mu': 0.0, 'sigma': 0.978535583741794}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3274` (run through the slot wrapper).
Returned: `{'mu': '0', 'nu': '14.412438143726687', 'sigma': '0.97853558374179395'}`, status `0`, logLik `-7.49007497404189592771217576228082180023193359375`.
Independent logLik at that point: `-7.490074974041899768476444684` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.88e-07`.
Better feasible point: `[0.0, 14.41243543475385979321564999, 0.9785355837417939461886362551]`; logLik `-7.490074974041899098173698709`.
Stationary local point: `[0.0, 14.41243543475385979321564999, 0.9785355837417939461886362551]`; logLik `-7.490074974041899098173698709`. Verified coefficient relative error: `1.88e-07`.
Local information eigenvalue range: `[0.007589213256710717166982544198, 0.007589213256710717166982544198]`; per-observation stationarity residual `1.54894057475412407873117702e-53`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3275: ht, constant-1.5768-1e-08

Data: `[1.5767971558400922, 1.5767971716080642, 1.576797187376036, 1.5767972031440078, 1.5767972346799517]`. Holds: `{'mu': 0.0, 'sigma': 0.978535583741794}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3275` (run through the slot wrapper).
Returned: `{'mu': '0', 'nu': '14.420304139922104', 'sigma': '0.97853558374179395'}`, status `0`, logLik `-7.4898395860832938097928490606136620044708251953125`.
Independent logLik at that point: `-7.489839586083298221501673276` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.03e-08`.
Better feasible point: `[0.0, 14.42030472079167634974903807, 0.9785355837417939461886362551]`; logLik `-7.489839586083298190747871229`.
Stationary local point: `[0.0, 14.42030472079167634974903807, 0.9785355837417939461886362551]`; logLik `-7.489839586083298190747871229`. Verified coefficient relative error: `4.03e-08`.
Local information eigenvalue range: `[0.007581416547555996613495916425, 0.007581416547555996613495916425]`; per-observation stationarity residual `6.886526737816502347861483792e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3276: ht, constant-1.5768-1e-12

Data: `[1.5767971873728823, 1.5767971873744593, 1.576797187376036, 1.5767971873776128, 1.5767971873807665]`. Holds: `{'mu': 0.0, 'sigma': 0.978535583741794}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3276` (run through the slot wrapper).
Returned: `{'mu': '0', 'nu': '14.420309257814994', 'sigma': '0.97853558374179395'}`, status `0`, logLik `-7.4898395625561935418090797611512243747711181640625`.
Independent logLik at that point: `-7.489839562556198368696917687` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.6e-07`.
Better feasible point: `[0.0, 14.42030550604512802908379344, 0.9785355837417939461886362551]`; logLik `-7.489839562556197085735468149`.
Stationary local point: `[0.0, 14.42030550604512802908379344, 0.9785355837417939461886362551]`; logLik `-7.489839562556197085735468149`. Verified coefficient relative error: `2.6e-07`.
Local information eigenvalue range: `[0.007581420326381936354809752058, 0.007581420326381936354809752058]`; per-observation stationarity residual `2.085746967002331114330724598e-52`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3285: ht, limit-concentration

Data: `[1.5767940337816613, 1.5767956105788485, 1.576797187376036, 1.5767987641732233, 1.5768003409704108]`. Holds: `{'mu': 0.0, 'sigma': 0.978535583741794}`.
Call: `route_ht(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3285` (run through the slot wrapper).
Returned: `{'mu': '0', 'nu': '14.420308976075622', 'sigma': '0.97853558374179395'}`, status `0`, logLik `-7.48983956256201555135021408204920589923858642578125`.
Independent logLik at that point: `-7.489839562562019421617633725` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.41e-07`.
Better feasible point: `[0.0, 14.42030550469344538160715199, 0.9785355837417939461886362551]`; logLik `-7.48983956256201832325411479`.
Stationary local point: `[0.0, 14.42030550469344538160715199, 0.9785355837417939461886362551]`; logLik `-7.48983956256201832325411479`. Verified coefficient relative error: `2.41e-07`.
Local information eigenvalue range: `[0.007581420033000607970058057767, 0.007581420033000607970058057767]`; per-observation stationarity residual `1.120447361642103005565118341e-52`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ht/mle.hpp:22` (special difference): `return std::lgamma((nu+1)/2)-std::lgamma(nu/2)-0.5*(std::log(std::acos(-1.0))+std::log(nu));`. Execution-path causation remains open.

## Case 3370: huber, scale-1e+150

Data: `[-9.154959257525536e+149, 1.2042368104741093e+150, 8.955938399925289e+149, 6.264536210206275e+149, 5.382849470645169e+149, -1.411883776251043e+150, -2.071503240433249e+149, 4.5641296412776685e+149, 8.319221609464081e+149, -8.049986784936165e+149, -3.283278089777626e+149, -1.91262263850988e+150]`. Holds: `{'sigma': 1.089043634045728}`.
Call: `route_huber(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3370` (run through the slot wrapper).
Returned: `{'mu': '1.2463132004222096e+149', 'sigma': '1.0890436340457279', 'epsilon': '1.345'}`, status `0`, logLik `-12515018109074708728103147346763677479143791941969900292901537381867512956118321414541689654746318164389404760061594587512413230801534924514818810445824.0`.
Independent logLik at that point: `-1.25150181090747095997417408e+151` (230 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.01`.
Better feasible point: `[1.246313200422209649962638199e+149, 1.089043634045727904080536064, 1.331549999999999973621100935]`; logLik `-1.238986792798396250374432339e+151`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/huber/mle.hpp:67` (step stop): `const double d=std::abs(z)<epsilon ? 1 : (std::abs(z)==epsilon ? .5 : 0);`. Execution-path causation remains open.

## Case 3588: invburr, constant-1.12109-1e-12

Data: `[1.121092244780946, 1.121092244782067, 1.1210922447831881, 1.1210922447843092, 1.1210922447865515]`. Holds: `{'shape1': 0.855256299891619}`.
Call: `route_invburr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3588` (run through the slot wrapper).
Returned: `{'shape1': '0.855256299891619', 'shape2': '1019643203427.7635', 'scale': '1.1210922447835645'}`, status `0`, logLik `127.5472248826728218773496337234973907470703125`.
Independent logLik at that point: `127.5472566387118227621581106` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `2.49e-07`.
Stationary local point: `[0.8552562998916189984655034095, 1019650101014.353582104368785, 1.121092244783564432543830214]`; logLik `127.5472566419459726016752526`. Verified coefficient relative error: `—`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/invburr/mle.hpp:152` (step stop): `double scale0 = std::numeric_limits<double>::quiet_NaN(), double reltol = 1e-12) {`. Execution-path causation remains open.

## Case 3591: invburr, constant-1.12109-1e-12

Data: `[1.121092244780946, 1.121092244782067, 1.1210922447831881, 1.1210922447843092, 1.1210922447865515]`. Holds: `{'shape1': 0.855256299891619}`.
Call: `route_invburr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3591` (run through the slot wrapper).
Returned: `{'shape1': '0.855256299891619', 'shape2': '1019643203427.7635', 'scale': '1.1210922447835645'}`, status `0`, logLik `127.5472248826728218773496337234973907470703125`.
Independent logLik at that point: `127.5472566387118227621581106` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `2.49e-07`.
Stationary local point: `[0.8552562998916189984655034095, 1019650101014.353582104368785, 1.121092244783564432543830214]`; logLik `127.5472566419459726016752526`. Verified coefficient relative error: `—`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/invburr/mle.hpp:152` (step stop): `double scale0 = std::numeric_limits<double>::quiet_NaN(), double reltol = 1e-12) {`. Execution-path causation remains open.

## Case 3759: invgamma, constant-1.4716-1e-12

Data: `[1.471602327564204, 1.4716023275656755, 1.4716023275671473, 1.471602327568619, 1.4716023275715622]`. Holds: `{}`.
Call: `route_invgamma(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3759` (run through the slot wrapper).
Returned: `{'alpha': '3.3783923073114094e+23', 'beta': '4.9716499828754097e+23'}`, status `0`, logLik `126.4156909206440815296446089632809162139892578125`.
Independent logLik at that point: `126.4155640742171641268767892` (103 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `1e-06`.
Stationary local point: `[337822090175699254729020.5417, 497139774206257170746826.7789]`; logLik `126.4155640778390147214762587`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/invgamma/mle.hpp:49`: Reciprocals are rounded to double before the gamma fit (line 53); nearly constant transformed data lose likelihood information.

## Case 3762: invgamma, constant-1.4716-1e-12

Data: `[1.471602327564204, 1.4716023275656755, 1.4716023275671473, 1.471602327568619, 1.4716023275715622]`. Holds: `{}`.
Call: `route_invgamma(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3762` (run through the slot wrapper).
Returned: `{'alpha': '3.3783923073114094e+23', 'beta': '4.9716499828754097e+23'}`, status `0`, logLik `126.4156909206440815296446089632809162139892578125`.
Independent logLik at that point: `126.4155640742171641268767892` (103 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `1e-06`.
Stationary local point: `[337822090175699254729020.5417, 497139774206257170746826.7789]`; logLik `126.4155640778390147214762587`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/invgamma/mle.hpp:49`: Reciprocals are rounded to double before the gamma fit (line 53); nearly constant transformed data lose likelihood information.

## Case 3835: invgauss, constant-2.43772-1e-12

Data: `[2.4377209513444593, 2.4377209513468974, 2.437720951349335, 2.4377209513517726, 2.4377209513566482]`. Holds: `{}`.
Call: `route_invgauss(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3835` (run through the slot wrapper).
Returned: `{'mean': '2.4377209513498226', 'shape': '8.2353088627643268e+23'}`, status `0`, logLik `123.8920506050393868235914851538836956024169921875`.
Independent logLik at that point: `123.8920506289928638800950107` (103 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1e-08`.
Better feasible point: `[2.437720951349822495757280194, 823530894536327964698353.9771]`; logLik `123.8920506301140259921499671`.
Stationary local point: `[2.437720951349822495757280194, 823530894536327964698353.9771]`; logLik `123.8920506301140259921499671`. Verified coefficient relative error: `1e-08`.
Local information eigenvalue range: `[0.4999999899701452614340619354, 8.44613722770311747448560006]`; per-observation stationarity residual `8.18079216495713151721729586e-76`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/invgauss/mle.hpp:26`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 3838: invgauss, constant-2.43772-1e-12

Data: `[2.4377209513444593, 2.4377209513468974, 2.437720951349335, 2.4377209513517726, 2.4377209513566482]`. Holds: `{}`.
Call: `route_invgauss(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 3838` (run through the slot wrapper).
Returned: `{'mean': '2.4377209513498226', 'shape': '8.2353088627643268e+23'}`, status `0`, logLik `123.8920506050393868235914851538836956024169921875`.
Independent logLik at that point: `123.8920506289928638800950107` (103 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1e-08`.
Better feasible point: `[2.437720951349822495757280194, 823530894536327964698353.9771]`; logLik `123.8920506301140259921499671`.
Stationary local point: `[2.437720951349822495757280194, 823530894536327964698353.9771]`; logLik `123.8920506301140259921499671`. Verified coefficient relative error: `1e-08`.
Local information eigenvalue range: `[0.4999999899701452614340619354, 8.44613722770311747448560006]`; per-observation stationarity residual `8.18079216495713151721729586e-76`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/invgauss/mle.hpp:26`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 4217: invweibull, constant-0.50717-0.0001

Data: `[0.5070683006125823, 0.5071190175860383, 0.5071697345594942, 0.5072204515329501, 0.507321885479862]`. Holds: `{}`.
Call: `route_invweibull(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4217` (run through the slot wrapper).
Returned: `{'shape': '7076.604257907491', 'scale': '0.50713823195394825'}`, status `0`, logLik `39.81219762261731176522516761906445026397705078125`.
Independent logLik at that point: `39.81219762261723910227563295` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.21e-07`.
Better feasible point: `[7076.606527224668836485047142, 0.5071382319443191373666843657]`; logLik `39.8121976226176397498289988`.
Stationary local point: `[7076.606527224668836485047142, 0.5071382319443191373666843657]`; logLik `39.8121976226176397498289988`. Verified coefficient relative error: `3.21e-07`.
Local information eigenvalue range: `[1.5584103494586289419963981, 50078360.11862153283776378947]`; per-observation stationarity residual `1.772733108153545557366251149e-49`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/invweibull/mle.hpp:29`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 4218: invweibull, constant-0.50717-1e-08

Data: `[0.5071697244160995, 0.5071697294877968, 0.5071697345594942, 0.5071697396311916, 0.5071697497745863]`. Holds: `{}`.
Call: `route_invweibull(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4218` (run through the slot wrapper).
Returned: `{'shape': '70766390.784500614', 'scale': '0.50716973140977695'}`, status `0`, logLik `85.8639263550397373592204530723392963409423828125`.
Independent logLik at that point: `85.86392635209075635721427323` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.19e-07`.
Better feasible point: `[70766413.34268498369164489736, 0.5071697314097760153055155365]`; logLik `85.86392635209115226378793716`.
Stationary local point: `[70766413.34268498369164489736, 0.5071697314097760153055155365]`; logLik `85.86392635209115226378793716`. Verified coefficient relative error: `3.19e-07`.
Local information eigenvalue range: `[1.558346581745978210411737789, 5007885257387761.86437890138]`; per-observation stationarity residual `1.357901392729032602030049483e-45`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/invweibull/mle.hpp:29`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 4219: invweibull, constant-0.50717-1e-12

Data: `[0.5071697345584799, 0.5071697345589871, 0.5071697345594942, 0.5071697345600014, 0.5071697345610158]`. Holds: `{}`.
Call: `route_invweibull(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4219` (run through the slot wrapper).
Returned: `{'shape': '707679756578.31445', 'scale': '0.50716973455917924'}`, status `0`, logLik `131.915719942556307842096430249512195587158203125`.
Independent logLik at that point: `131.9156810176477241109381974` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `2.95e-07`.
Stationary local point: `[707675622377.0811810363863884, 0.5071697345591792610125881859]`; logLik `131.9156810192935459506376014`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/invweibull/mle.hpp:29`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 4220: invweibull, constant-1-0.0001

Data: `[0.9998, 0.9999, 1.0, 1.0001, 1.0003]`. Holds: `{}`.
Call: `route_invweibull(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4220` (run through the slot wrapper).
Returned: `{'shape': '7076.6042579060713', 'scale': '0.99993788547817564'}`, status `0`, logLik `36.417649876369324601910193450748920440673828125`.
Independent logLik at that point: `36.41764987636932724601705418` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.21e-07`.
Better feasible point: `[7076.606527223181858678908094, 0.9999378854591896281638560773]`; logLik `36.41764987636972789354667226`.
Stationary local point: `[7076.606527223181858678908094, 0.9999378854591896281638560773]`; logLik `36.41764987636972789354667226`. Verified coefficient relative error: `3.21e-07`.
Local information eigenvalue range: `[1.558410349458658577683119236, 50078360.11860049079316899159]`; per-observation stationarity residual `1.772756368998366447593493666e-49`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/invweibull/mle.hpp:29`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 4221: invweibull, constant-1-1e-08

Data: `[0.99999998, 0.99999999, 1.0, 1.00000001, 1.00000003]`. Holds: `{}`.
Call: `route_invweibull(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4221` (run through the slot wrapper).
Returned: `{'shape': '70766390.875483871', 'scale': '0.99999999378961912'}`, status `0`, logLik `82.4693786155012276140041649341583251953125`.
Independent logLik at that point: `82.46937861550122992116278559` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.19e-07`.
Better feasible point: `[70766413.46901024572271312553, 0.9999999937896171841333414369]`; logLik `82.46937861550162706832142764`.
Stationary local point: `[70766413.46901024572271312553, 0.9999999937896171841333414369]`; logLik `82.46937861550162706832142764`. Verified coefficient relative error: `3.19e-07`.
Local information eigenvalue range: `[1.55834658137616635974470119, 5007885275266934.322601800621]`; per-observation stationarity residual `2.023608740222697565431289851e-45`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/invweibull/mle.hpp:29`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 4222: invweibull, constant-1-1e-12

Data: `[0.999999999998, 0.999999999999, 1.0, 1.000000000001, 1.000000000003]`. Holds: `{}`.
Call: `route_invweibull(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4222` (run through the slot wrapper).
Returned: `{'shape': '707662919250.82947', 'scale': '0.99999999999937894'}`, status `0`, logLik `128.521064959205858713175985030829906463623046875`.
Independent logLik at that point: `128.5210649551231128049422012` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.19e-07`.
Better feasible point: `[707663145134.7851832582397241, 0.9999999999993789981571690525]`; logLik `128.5210649592062567423664003`.
Stationary local point: `[707663145134.7851832582397241, 0.9999999999993789981571690525]`; logLik `128.5210649592062567423664003`. Verified coefficient relative error: `3.19e-07`.
Local information eigenvalue range: `[1.558338753862740808070352466, 500787126982055981379275.3566]`; per-observation stationarity residual `1.140850315334705877934126961e-48`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/invweibull/mle.hpp:29`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 4283: invweibull, lower-1e-200

Data: `[1e-200, 2e-200, 3e-200, 5e-200, 9e-200]`. Holds: `{'shape': 1.928819454840455, 'scale': 0.3434414370956311}`.
Call: `route_invweibull(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4283` (run through the slot wrapper).
Returned: `{'shape': '1.9288194548404549', 'scale': '0.34344143709563107'}`, status `0`, logLik `-inf`.
Independent logLik at that point: `-1.0656757388605999270237015e+385` (280 digits).
Evidence: status Ok with nonfinite reported logLik. Worst relative discrepancy: `inf`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/invweibull/mle.hpp:29`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 4301: kumar, lower-1e-12

Data: `[1e-12, 2e-12, 3e-12, 5e-12, 9e-12]`. Holds: `{}`.
Call: `route_kumar(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4301` (run through the slot wrapper).
Returned: `{'a': '1.4811824612439668', 'b': '65142298124747376'}`, status `0`, logLik `126.7570047360390361745885456912219524383544921875`.
Independent logLik at that point: `126.757004736039064358799871` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.12e-08`.
Better feasible point: `[1.481182460811019798463986655, 65142297396084900.06384042335]`; logLik `126.7570047360390643591535777`.
Stationary local point: `[1.481182460811019798463986655, 65142297396084900.06384042335]`; logLik `126.7570047360390643591535777`. Verified coefficient relative error: `1.12e-08`.
Local information eigenvalue range: `[0.001128729569585996130851667697, 1467.104377877090695736200086]`; per-observation stationarity residual `2.489707692130806821888907654e-58`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/kumar/mle.hpp:158` (special difference): `const double b = inverse_digamma_scalar(-0.5772156649015328606 - a0 * mu,maxit);`. Execution-path causation remains open.

## Case 4303: kumar, lower-1e-17

Data: `[1e-17, 2e-17, 3e-17, 5.0000000000000005e-17, 9.000000000000001e-17]`. Holds: `{}`.
Call: `route_kumar(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4303` (run through the slot wrapper).
Returned: `{'a': '1.4811824627136712', 'b': '1.6587288786210314e+24'}`, status `0`, logLik `184.3216320608902378808124922215938568115234375`.
Independent logLik at that point: `184.3216320608902057961824564` (104 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `7.11e-08`.
Better feasible point: `[1.481182460811019748809394207, 1658728760747840185595962.802]`; logLik `184.3216320608902058030135816`.
Stationary local point: `[1.481182460811019748809394207, 1658728760747840185595962.802]`; logLik `184.3216320608902058030135816`. Verified coefficient relative error: `7.11e-08`.
Local information eigenvalue range: `[0.0005406252165704554712038110105, 3063.054115524417421901777608]`; per-observation stationarity residual `4.067053616958354646082610825e-51`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/kumar/mle.hpp:158` (special difference): `const double b = inverse_digamma_scalar(-0.5772156649015328606 - a0 * mu,maxit);`. Execution-path causation remains open.

## Case 4435: lgamma, constant-1.95363-1e-12

Data: `[1.953632517309603, 1.9536325173115567, 1.9536325173135103, 1.9536325173154638, 1.9536325173193712]`. Holds: `{}`.
Call: `route_lgamma(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4435` (run through the slot wrapper).
Returned: `{'shapelog': '1.5151883330149515e+23', 'ratelog': '2.2625203777481562e+23'}`, status `0`, logLik `124.99904549368562811650917865335941314697265625`.
Independent logLik at that point: `124.999010603248488632364611` (103 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `2.79e-07`.
Stationary local point: `[151516718813491601451276.6988, 226248880363890310626472.161]`; logLik `124.9990106052379068162476149`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/lgamma/mle.hpp:23`: Log-transformed observations are rounded before gamma fitting (line 28). A stable base gamma solver does not recover information already lost in the transformation.

## Case 4438: lgamma, constant-1.95363-1e-12

Data: `[1.953632517309603, 1.9536325173115567, 1.9536325173135103, 1.9536325173154638, 1.9536325173193712]`. Holds: `{}`.
Call: `route_lgamma(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4438` (run through the slot wrapper).
Returned: `{'shapelog': '1.5151883330149515e+23', 'ratelog': '2.2625203777481562e+23'}`, status `0`, logLik `124.99904549368562811650917865335941314697265625`.
Independent logLik at that point: `124.999010603248488632364611` (103 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `2.79e-07`.
Stationary local point: `[151516718813491601451276.6988, 226248880363890310626472.161]`; logLik `124.9990106052379068162476149`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/lgamma/mle.hpp:23`: Log-transformed observations are rounded before gamma fitting (line 28). A stable base gamma solver does not recover information already lost in the transformation.

## Case 4525: llogis, constant-1.30704-1e-12

Data: `[1.307038121314793, 1.3070381213161002, 1.3070381213174072, 1.307038121318714, 1.3070381213213282]`. Holds: `{}`.
Call: `route_llogis(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4525` (run through the slot wrapper).
Returned: `{'shape': '977338509062.41064', 'scale': '1.3070381213175295'}`, status `0`, logLik `126.834436725513342025806196033954620361328125`.
Independent logLik at that point: `126.834433670742425445843778` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `2.41e-08`.
Stationary local point: `[977338636898.3332872121030156, 1.307038121317529612059144421]`; logLik `126.8344336763737852695241008`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/llogis/mle.hpp:29`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 4528: llogis, constant-1.30704-1e-12

Data: `[1.307038121314793, 1.3070381213161002, 1.3070381213174072, 1.307038121318714, 1.3070381213213282]`. Holds: `{}`.
Call: `route_llogis(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4528` (run through the slot wrapper).
Returned: `{'shape': '977338509062.41064', 'scale': '1.3070381213175295'}`, status `0`, logLik `126.834436725513342025806196033954620361328125`.
Independent logLik at that point: `126.834433670742425445843778` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `2.41e-08`.
Stationary local point: `[977338636898.3332872121030156, 1.307038121317529612059144421]`; logLik `126.8344336763737852695241008`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/llogis/mle.hpp:29`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 4535: llogis, limit-outlier

Data: `[1.4612222342357195, 0.7038909590134532, 0.013073080872494258, 1.3070381213174072, 1307058.4295823444]`. Holds: `{}`.
Call: `route_llogis(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4535` (run through the slot wrapper).
Returned: `{'shape': '0.30409014489506558', 'scale': '2.2019824895998328'}`, status `0`, logLik `-26.211246724803888952237684861756861209869384765625`.
Independent logLik at that point: `-26.21124672480388987880114439` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.29e-08`.
Better feasible point: `[0.3040901444416098056471680809, 2.201982518014344666247509704]`; logLik `-26.21124672480388986152180875`.
Stationary local point: `[0.3040901444416098056471680809, 2.201982518014344666247509704]`; logLik `-26.21124672480388986152180875`. Verified coefficient relative error: `1.29e-08`.
Local information eigenvalue range: `[0.03216148610146161732490684984, 1.267277914190413587771949419]`; per-observation stationarity residual `6.989469683529492268909581257e-64`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/llogis/mle.hpp:29`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 4601: lnorm, constant-1.44519-1e-12

Data: `[1.445187510724387, 1.445187510725832, 1.4451875107272774, 1.4451875107287226, 1.445187510731613]`. Holds: `{}`.
Call: `route_lnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4601` (run through the slot wrapper).
Returned: `{'meanlog': '0.36823907835500752', 'sdlog': '1.7205031402446402e-12'}`, status `0`, logLik `126.5061336646104592773554031737148761749267578125`.
Independent logLik at that point: `126.5061550621891484055802785` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `1.69e-07`.
Stationary local point: `[0.3682390783550075237994203856, 1.720495777306061532318569168e-12]`; logLik `126.5061550622881297396430773`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/lnorm/mle.hpp:27`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 4604: lnorm, constant-1.44519-1e-12

Data: `[1.445187510724387, 1.445187510725832, 1.4451875107272774, 1.4451875107287226, 1.445187510731613]`. Holds: `{}`.
Call: `route_lnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4604` (run through the slot wrapper).
Returned: `{'meanlog': '0.36823907835500752', 'sdlog': '1.7205031402446402e-12'}`, status `0`, logLik `126.5061336646104592773554031737148761749267578125`.
Independent logLik at that point: `126.5061550621891484055802785` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `1.69e-07`.
Stationary local point: `[0.3682390783550075237994203856, 1.720495777306061532318569168e-12]`; logLik `126.5061550622881297396430773`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/lnorm/mle.hpp:27`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 4611: lnorm, offset-1e8

Data: `[100000001.46643509, 100000000.44635502, 100000001.90765181, 100000002.98407541, 100000001.44518751, 100000000.48315705, 100000001.20661016, 100000000.26392101, 100000004.95617464, 100000000.76818824, 100000001.8055238, 100000000.58251828]`. Holds: `{}`.
Call: `route_lnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4611` (run through the slot wrapper).
Returned: `{'meanlog': '18.420680759215529', 'sdlog': '1.2760092561094825e-08'}`, status `0`, logLik `-19.952111848421054673963226377964019775390625`.
Independent logLik at that point: `-19.95211154831220293281132615` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `1.5e-08`.
Stationary local point: `[18.42068075921553028625384347, 0.000000012760092241976638395001918]`; logLik `-19.95211154831209190654545438`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/lnorm/mle.hpp:27`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 4745: logitnorm, constant-0.840122-1e-12

Data: `[0.8401216942096138, 0.8401216942104539, 0.840121694211294, 0.8401216942121341, 0.8401216942138143]`. Holds: `{}`.
Call: `route_logitnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4745` (run through the slot wrapper).
Returned: `{'mu': '1.6591338183876283', 'sigma': '1.0760888594022741e-11'}`, status `0`, logLik `129.218576569267696640963549725711345672607421875`.
Independent logLik at that point: `129.218577178016999635185947` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.22e-07`.
Better feasible point: `[1.65913381838762826655450985, 1.076088728383153363979232971e-11]`; logLik `129.2185771780423684121698007`.
Stationary local point: `[1.65913381838762826655450985, 1.076088728383153363979232971e-11]`; logLik `129.2185771780423684121698007`. Verified coefficient relative error: `1.22e-07`.
Local information eigenvalue range: `[0.1523743168476911797160767717, 2.00000048701979758919964919]`; per-observation stationarity residual `2.133247250507913770902771884e-44`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/logitnorm/mle.hpp:27`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 4748: logitnorm, constant-0.25-1e-12

Data: `[0.2499999999995, 0.24999999999975, 0.25, 0.25000000000025, 0.25000000000075]`. Holds: `{}`.
Call: `route_logitnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4748` (run through the slot wrapper).
Returned: `{'mu': '-1.0986122886678431', 'sigma': '2.2939990256907834e-12'}`, status `0`, logLik `135.278812111544965546272578649222850799560546875`.
Independent logLik at that point: `135.2788871194411641392509172` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `5.54e-07`.
Stationary local point: `[-1.098612288667842971415810623, 2.293964607844937311976664576e-12]`; logLik `135.2788871292403955663590305`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/logitnorm/mle.hpp:27`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 4818: lomax, ties

Data: `[0.09289236940674983, 0.09289236940674983, 0.09289236940674983, 0.09289236940674983, 5.449695330117521e-05, 1.057578062299887]`. Holds: `{}`.
Call: `route_lomax(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4818` (run through the slot wrapper).
Returned: `{'lambda': '6.6360638400476306', 'kappa': '1.4988855653281876'}`, status `0`, logLik `3.78047117307156810284141101874411106109619140625`.
Independent logLik at that point: `3.780471173071569182185199702` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.98e-08`.
Better feasible point: `[6.636064037563212416185463043, 1.49888553856847895511097166]`; logLik `3.780471173071569391768866092`.
Stationary local point: `[6.636064037563212416185463043, 1.49888553856847895511097166]`; logLik `3.780471173071569391768866092`. Verified coefficient relative error: `2.98e-08`.
Local information eigenvalue range: `[0.05707987687565728922526742306, 1.381565694112290055601141281]`; per-observation stationarity residual `3.200819684930883717562944072e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/lomax/certificate.hpp:118` (step stop): `r.valid=std::abs(std::log(r.lambda)+d.logmean-center.z)<1e-8 &&`. Execution-path causation remains open.

## Case 4831: lomax, limit-outlier

Data: `[0.09289236940674983, 5.449695330117521e-05, 1.057578062299887, 1.2436453068630677, 1000002.5988071763]`. Holds: `{}`.
Call: `route_lomax(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4831` (run through the slot wrapper).
Returned: `{'lambda': '24225.55600773757', 'kappa': '0.094453574136279764'}`, status `0`, logLik `-19.2584765626710492369966232217848300933837890625`.
Independent logLik at that point: `-19.25847656267106011411357555` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.52e-08`.
Better feasible point: `[24225.55637607257077254559709, 0.09445357401234056559153953118]`; logLik `-19.25847656267106008732903624`.
Stationary local point: `[24225.55637607257077254559709, 0.09445357401234056559153953118]`; logLik `-19.25847656267106008732903624`. Verified coefficient relative error: `1.52e-08`.
Local information eigenvalue range: `[0.04598628200803141094823755208, 1.007807061797046095826371344]`; per-observation stationarity residual `3.82232765800228739107218943e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/lomax/certificate.hpp:118` (step stop): `r.valid=std::abs(std::log(r.lambda)+d.logmean-center.z)<1e-8 &&`. Execution-path causation remains open.

## Case 4906: lst, interior

Data: `[0.6897161493290532, 0.01677922609010563, -0.4551346933365129, 2.307805913878299, 0.21769109825867097, -3.1232635003953715, 0.21939984827817252, 1.1797767050200958, 0.8721420016898619, -0.23169264598136521, 0.9191905308577012, 0.4046314572322073]`. Holds: `{'mu': 0.0745507089520688}`.
Call: `route_lst(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4906` (run through the slot wrapper).
Returned: `{'mu': '0.074550708952068806', 'sigma': '0.67742526750546095', 'df': '2.0879119072002905'}`, status `0`, logLik `-18.56455166246897903192802914418280124664306640625`.
Independent logLik at that point: `-18.56455166246898166972330903` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `7.71e-08`.
Better feasible point: `[0.07455070895206880599026533218, 0.6774252490653741479667467537, 2.087911746181999079073118686]`; logLik `-18.56455166246897714612324605`.
Stationary local point: `[0.07455070895206880599026533218, 0.6774252490653741479667467537, 2.087911746181999079073118686]`; logLik `-18.56455166246897714612324605`. Verified coefficient relative error: `7.71e-08`.
Local information eigenvalue range: `[0.1107314277979534739031327251, 0.9075061821722436507549667707]`; per-observation stationarity residual `5.370715932075927675656239376e-56`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/lst/mle.hpp:42` (special difference): `cppunivariate::special::lgamma((df + 1.0) / 2.0) -`. Execution-path causation remains open.

## Case 4909: lst, tiny-5

Data: `[0.6897161493290532, 0.01677922609010563, -0.4551346933365129, 2.307805913878299, 0.21769109825867097]`. Holds: `{'mu': 0.0745507089520688}`.
Call: `route_lst(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4909` (run through the slot wrapper).
Returned: `{'mu': '0.074550708952068806', 'sigma': '0.48191799366440213', 'df': '1.5901041872019417'}`, status `0`, logLik `-6.88306640507769618153588453424163162708282470703125`.
Independent logLik at that point: `-6.883066405077697783535774404` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `7.07e-08`.
Better feasible point: `[0.07455070895206880599026533218, 0.4819179780329452842819009685, 1.590104074728892998461650401]`; logLik `-6.883066405077695803868712682`.
Stationary local point: `[0.07455070895206880599026533218, 0.4819179780329452842819009685, 1.590104074728892998461650401]`; logLik `-6.883066405077695803868712682`. Verified coefficient relative error: `7.07e-08`.
Local information eigenvalue range: `[0.1254622710292409362648307797, 0.8079371943974872034602967177]`; per-observation stationarity residual `1.732830429875360868944296994e-56`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/lst/mle.hpp:42` (special difference): `cppunivariate::special::lgamma((df + 1.0) / 2.0) -`. Execution-path causation remains open.

## Case 4918: lst, scale-1e+150

Data: `[6.897161493290532e+149, 1.6779226090105628e+148, -4.551346933365129e+149, 2.3078059138782988e+150, 2.17691098258671e+149, -3.1232635003953716e+150, 2.1939984827817252e+149, 1.1797767050200958e+150, 8.72142001689862e+149, -2.316926459813652e+149, 9.191905308577011e+149, 4.046314572322073e+149]`. Holds: `{'mu': 0.0745507089520688}`.
Call: `route_lst(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4918` (run through the slot wrapper).
Returned: `{'mu': '0.074550708952068806', 'sigma': '7.4583794493400869e+149', 'df': '2.3411550885394927'}`, status `0`, logLik `-4163.672407047453816630877554416656494140625`.
Independent logLik at that point: `-4163.672407047454500061746663` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `7.5e-07`.
Better feasible point: `[0.07455070895206880599026533218, 7.458377562944281227784664108e+149, 2.34115333255073514405309381]`; logLik `-4163.672407047454163374135376`.
Stationary local point: `[0.07455070895206880599026533218, 7.458377562944281227784664108e+149, 2.34115333255073514405309381]`; logLik `-4163.672407047454163374135376`. Verified coefficient relative error: `7.5e-07`.
Local information eigenvalue range: `[0.08849329210988070551460458606, 0.9433732660828583271992240282]`; per-observation stationarity residual `5.76486956891973965689095114e-48`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/lst/mle.hpp:42` (special difference): `cppunivariate::special::lgamma((df + 1.0) / 2.0) -`. Execution-path causation remains open.

## Case 4922: lst, interior

Data: `[0.6897161493290532, 0.01677922609010563, -0.4551346933365129, 2.307805913878299, 0.21769109825867097, -3.1232635003953715, 0.21939984827817252, 1.1797767050200958, 0.8721420016898619, -0.23169264598136521, 0.9191905308577012, 0.4046314572322073]`. Holds: `{'sigma': 1.0270947160838733}`.
Call: `route_lst(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4922` (run through the slot wrapper).
Returned: `{'mu': '0.40062216773021386', 'sigma': '1.0270947160838733', 'df': '3.9204859516444648'}`, status `0`, logLik `-18.4936131939076773278429755009710788726806640625`.
Independent logLik at that point: `-18.49361319390767984163183696` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.35e-08`.
Better feasible point: `[0.4006221542019311456148440676, 1.027094716083873260359382584, 3.920485938188740959356356521]`; logLik `-18.49361319390767891138587467`.
Stationary local point: `[0.4006221542019311456148440676, 1.027094716083873260359382584, 3.920485938188740959356356521]`; logLik `-18.49361319390767891138587467`. Verified coefficient relative error: `1.35e-08`.
Local information eigenvalue range: `[0.09564183421766251436898596791, 24.4849832554430524723260157]`; per-observation stationarity residual `1.198730044043311549326813588e-66`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/lst/mle.hpp:42` (special difference): `cppunivariate::special::lgamma((df + 1.0) / 2.0) -`. Execution-path causation remains open.

## Case 4938: lst, interior

Data: `[0.6897161493290532, 0.01677922609010563, -0.4551346933365129, 2.307805913878299, 0.21769109825867097, -3.1232635003953715, 0.21939984827817252, 1.1797767050200958, 0.8721420016898619, -0.23169264598136521, 0.9191905308577012, 0.4046314572322073]`. Holds: `{'mu': 0.0745507089520688, 'sigma': 1.0270947160838733}`.
Call: `route_lst(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4938` (run through the slot wrapper).
Returned: `{'mu': '0.074550708952068806', 'sigma': '1.0270947160838733', 'df': '4.2762115797764979'}`, status `0`, logLik `-19.013619097211286401716279215179383754730224609375`.
Independent logLik at that point: `-19.01361909721129365574662559` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `7.14e-08`.
Better feasible point: `[0.07455070895206880599026533218, 1.027094716083873260359382584, 4.276211274324095093069907673]`; logLik `-19.01361909721129114568537863`.
Stationary local point: `[0.07455070895206880599026533218, 1.027094716083873260359382584, 4.276211274324095093069907673]`; logLik `-19.01361909721129114568537863`. Verified coefficient relative error: `7.14e-08`.
Local information eigenvalue range: `[0.08199066161038440803865838142, 0.08199066161038440803865838142]`; per-observation stationarity residual `1.063016220350046538378669146e-56`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/lst/mle.hpp:42` (special difference): `cppunivariate::special::lgamma((df + 1.0) / 2.0) -`. Execution-path causation remains open.

## Case 4951: lst, offset-1e8

Data: `[100000000.68971615, 100000000.01677923, 99999999.54486531, 100000002.30780591, 100000000.2176911, 99999996.8767365, 100000000.21939985, 100000001.1797767, 100000000.872142, 99999999.76830736, 100000000.91919053, 100000000.40463145]`. Holds: `{'mu': 0.0745507089520688, 'sigma': 1.0270947160838733}`.
Call: `route_lst(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 4951` (run through the slot wrapper).
Returned: `{'mu': '0.074550708952068806', 'sigma': '1.0270947160838733', 'df': '0.049882168324496358'}`, status `0`, logLik `-277.65376252056051953331916593015193939208984375`.
Independent logLik at that point: `-277.6537625205605365759628174` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.09e-08`.
Better feasible point: `[0.07455070895206880599026533218, 1.027094716083873260359382584, 0.04988216678372320088679992967]`; logLik `-277.6537625205605310047602647`.
Stationary local point: `[0.07455070895206880599026533218, 1.027094716083873260359382584, 0.04988216678372320088679992967]`; logLik `-277.6537625205605310047602647`. Verified coefficient relative error: `3.09e-08`.
Local information eigenvalue range: `[0.9732198243071339665916117272, 0.9732198243071339665916117272]`; per-observation stationarity residual `8.925583367826411807688627074e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/lst/mle.hpp:42` (special difference): `cppunivariate::special::lgamma((df + 1.0) / 2.0) -`. Execution-path causation remains open.

## Case 5020: naka, tiny-3

Data: `[1.1870206453189427, 1.8063039875699989, 0.4693270808393411]`. Holds: `{}`.
Call: `route_naka(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 5020` (run through the slot wrapper).
Returned: `{'shape': '1.1698574268692128', 'scale': '1.6306733389112849'}`, status `0`, logLik `-2.359056308828765491369949813815765082836151123046875`.
Independent logLik at that point: `-2.359056308828765965121547613` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.12e-08`.
Better feasible point: `[1.169857439936931336014932058, 1.630673338911285078385596842]`; logLik `-2.35905630882876584764027738`.
Stationary local point: `[1.169857439936931336014932058, 1.630673338911285078385596842]`; logLik `-2.35905630882876584764027738`. Verified coefficient relative error: `1.12e-08`.
Local information eigenvalue range: `[0.6276878180370939266156254206, 1.169857439936931012808314361]`; per-observation stationarity residual `2.676453154685401946718738464e-64`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/naka/mle.hpp:45`: Squared observations are rounded before gamma fitting (line 51). Squaring also underflows on 1e-200 samples, causing loud TransformFailure.

## Case 5025: naka, constant-1.61568-1e-12

Data: `[1.6156758120186856, 1.6156758120203014, 1.615675812021917, 1.6156758120235326, 1.615675812026764]`. Holds: `{}`.
Call: `route_naka(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 5025` (run through the slot wrapper).
Returned: `{'shape': '8.4459132122134825e+22', 'scale': '2.6104083295537253'}`, status `0`, logLik `125.9486634105689830676055862568318843841552734375`.
Independent logLik at that point: `125.9486862541624275228348273` (102 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `1.81e-07`.
Stationary local point: `[84459903924022794382471.19718, 2.610408329553725030778205682]`; logLik `125.9486862558890402142258376`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/naka/mle.hpp:45`: Squared observations are rounded before gamma fitting (line 51). Squaring also underflows on 1e-200 samples, causing loud TransformFailure.

## Case 5028: naka, constant-1.61568-1e-12

Data: `[1.6156758120186856, 1.6156758120203014, 1.615675812021917, 1.6156758120235326, 1.615675812026764]`. Holds: `{}`.
Call: `route_naka(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 5028` (run through the slot wrapper).
Returned: `{'shape': '8.4459132122134825e+22', 'scale': '2.6104083295537253'}`, status `0`, logLik `125.9486634105689830676055862568318843841552734375`.
Independent logLik at that point: `125.9486862541624275228348273` (102 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `1.81e-07`.
Stationary local point: `[84459903924022794382471.19718, 2.610408329553725030778205682]`; logLik `125.9486862558890402142258376`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/naka/mle.hpp:45`: Squared observations are rounded before gamma fitting (line 51). Squaring also underflows on 1e-200 samples, causing loud TransformFailure.

## Case 5348: nsbeta, scale-1e-150

Data: `[3.279712609634442e-151, 7.163848556685424e-151, 3.247469053643058e-151, 1.3428490451384639e-151, 5.302279618623525e-151, 5.9296803360997277e-151, 5.249832294577309e-151, 3.503576195388067e-151, 5.430978029647745e-151, 7.340437563236251e-151, 8.133244607203068e-152, 2.243783898794992e-151]`. Holds: `{'shape1': 2.0063421787377256, 'min': 0.0, 'max': 1.0}`.
Call: `route_nsbeta(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 5348` (run through the slot wrapper).
Returned: `{'shape1': '2.0063421787377256', 'shape2': '427914928881440.94', 'min': '0', 'max': '1'}`, status `0`, logLik `-3367.3720572377251301077194511890411376953125`.
Independent logLik at that point: `-3372.28168463238905959417178` (231 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.0595`.
Better feasible point: `[2.006342178737725578940853666, 1752739548698384140.485610965, 0.0, 1.0]`; logLik `-3172.022263514389523105504013`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/nsbeta/mle.hpp:24`: The held-shape score subtracts nearby digammas; line 25 also differences lgammas. The helper call omits maxit and uses its default inner budget. This bypasses the repaired held-shape beta implementation.

## Case 5591: pareto3, constant-0.687578-1e-12

Data: `[0.6875776168638714, 0.687577616864559, 0.6875776168652465, 0.6875776168659341, 0.6875776168673092]`. Holds: `{'min': 0.0}`.
Call: `route_pareto3(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 5591` (run through the slot wrapper).
Returned: `{'shape': '977349305550.53821', 'scale': '0.68757761686531094', 'min': '0'}`, status `0`, logLik `130.0462211103123308930662460625171661376953125`.
Independent logLik at that point: `130.0462304700327902467694759` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `7.2e-08`.
Stationary local point: `[977351135434.3642976093662224, 0.6875776168653109690504518427, 0.0]`; logLik `130.0462304717667031332246467`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/pareto3/mle.hpp:11`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 5666: pareto4, constant-57.9572-1e-12

Data: `[57.95719829085132, 57.95719829090928, 57.95719829096724, 57.9571982910252, 57.95719829114111]`. Holds: `{'shape1': 0.3157266252310832, 'min': 0.0}`.
Call: `route_pareto4(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 5666` (run through the slot wrapper).
Returned: `{'shape1': '0.31572662523108319', 'shape2': '1813810330713.4565', 'scale': '57.957198290888634', 'min': '0'}`, status `0`, logLik `108.0391749174143996015118318609893321990966796875`.
Independent logLik at that point: `108.0395364528419686066673019` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `3.35e-06`.
Stationary local point: `[0.3157266252310831911032096286, 1813936513691.650219906085776, 57.95719829088866339086097066, 0.0]`; logLik `108.0395367269637092811833284`. Verified coefficient relative error: `—`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/pareto4/mle.hpp:36` (legacy): `auto r=(!std::isnan(shape1)\|\|!std::isnan(shape2)) ? burr_mle_holds(span{y.data(),y.size()},shape1,shape2,scale,tol,maxit) : std::isnan(scale) ? burr_mle_legacy(span{y.data(),y.size()},tol,maxit) : burr_mle_fixed_scale(span{y.data(),y.size()},scale,tol,maxit);`. Execution-path causation remains open.

## Case 5669: pareto4, constant-57.9572-1e-12

Data: `[57.95719829085132, 57.95719829090928, 57.95719829096724, 57.9571982910252, 57.95719829114111]`. Holds: `{'shape1': 0.3157266252310832, 'min': 0.0}`.
Call: `route_pareto4(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 5669` (run through the slot wrapper).
Returned: `{'shape1': '0.31572662523108319', 'shape2': '1813810330713.4565', 'scale': '57.957198290888634', 'min': '0'}`, status `0`, logLik `108.0391749174143996015118318609893321990966796875`.
Independent logLik at that point: `108.0395364528419686066673019` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `3.35e-06`.
Stationary local point: `[0.3157266252310831911032096286, 1813936513691.650219906085776, 57.95719829088866339086097066, 0.0]`; logLik `108.0395367269637092811833284`. Verified coefficient relative error: `—`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/pareto4/mle.hpp:36` (legacy): `auto r=(!std::isnan(shape1)\|\|!std::isnan(shape2)) ? burr_mle_holds(span{y.data(),y.size()},shape1,shape2,scale,tol,maxit) : std::isnan(scale) ? burr_mle_legacy(span{y.data(),y.size()},tol,maxit) : burr_mle_fixed_scale(span{y.data(),y.size()},scale,tol,maxit);`. Execution-path causation remains open.

## Case 6026: sged, scale-1e+150

Data: `[6.449948853914544e+149, 5.314175321796899e+149, 1.0643637622867162e+149, 3.5784486371116986e+149, 4.912947510231833e+149, -1.4843850018264121e+150, -9.29136424879163e+149, -1.0582726819365746e+149, -8.768581195832416e+149, -1.8968909122061467e+149, 1.5580291981210541e+150, -1.2217397805116431e+150]`. Holds: `{'mean': -0.032496865700421936, 'sd': 0.9390299202739361}`.
Call: `route_sged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6026` (run through the slot wrapper).
Returned: `{'mean': '-0.032496865700421936', 'sd': '0.93902992027393606', 'nu': '1', 'xi': '0.95686239289789488'}`, status `0`, logLik `-12760733732762412112098049840268004421521170226614617189602315485226777188298601315609791374767771565051293701871773805728584280619110669836895104532480.0`.
Independent logLik at that point: `-1.276073373276241490299455565e+151` (230 digits).
Evidence: interior dimensionless free score exceeds 1e-3 with feasible ascent. Worst relative discrepancy: `2.19e-16`.
Better feasible point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 1.0, 0.956862382565068387294049206]`; logLik `-1.276073373276241268820350739e+151`.
Stationary local point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 1.0, 0.956862382565068387294049206]`; logLik `-1.276073373276241268820350739e+151`. Verified coefficient relative error: `—`.
Scaled free-coordinate score (xi): `-3.418313233452720418055493569e+142`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sged/face_payload.hpp:18` (special difference): `const double m=std::exp(special::lgamma(2/nu)-special::lgamma(1/nu));`. Execution-path causation remains open.

## Case 6029: sged, limit-concentration

Data: `[0.10643616335591917, 0.1064362697922954, 0.10643637622867162, 0.10643648266504785, 0.10643658910142409]`. Holds: `{'mean': -0.032496865700421936, 'sd': 0.9390299202739361}`.
Call: `route_sged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6029` (run through the slot wrapper).
Returned: `{'mean': '-0.032496865700421936', 'sd': '0.93902992027393606', 'nu': '1', 'xi': '0.89979592475428849'}`, status `0`, logLik `-1.3908154481007743985543356757261790335178375244140625`.
Independent logLik at that point: `-1.390815448100774325429319285` (70 digits).
Evidence: interior dimensionless free score exceeds 1e-3 with feasible ascent. Worst relative discrepancy: `3.04e-09`.
Better feasible point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 1.0, 0.8997959337522477372870544077]`; logLik `-1.390815443868899331372321216`.
Scaled free-coordinate score (xi): `0.08463747622062130631291135394`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sged/face_payload.hpp:18` (special difference): `const double m=std::exp(special::lgamma(2/nu)-special::lgamma(1/nu));`. Execution-path causation remains open.

## Case 6078: sged, interior

Data: `[0.6449948853914544, 0.5314175321796899, 0.10643637622867162, 0.35784486371116986, 0.4912947510231833, -1.4843850018264122, -0.929136424879163, -0.10582726819365745, -0.8768581195832416, -0.18968909122061467, 1.5580291981210541, -1.2217397805116432]`. Holds: `{'mean': -0.032496865700421936, 'sd': 0.9390299202739361, 'nu': 3.4208324500711944}`.
Call: `route_sged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6078` (run through the slot wrapper).
Returned: `{'mean': '-0.032496865700421936', 'sd': '0.93902992027393606', 'nu': '3.4208324500711944', 'xi': '2.3352191843687695'}`, status `0`, logLik `-14.692236368999591178408081759698688983917236328125`.
Independent logLik at that point: `-14.69223636899959559753804814` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.43e-08`.
Better feasible point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 3.420832450071194408280916832, 2.335219150946313706974981763]`; logLik `-14.69223636899959543877311569`.
Stationary local point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 3.420832450071194408280916832, 2.335219150946313706974981763]`; logLik `-14.69223636899959543877311569`. Verified coefficient relative error: `1.43e-08`.
Local information eigenvalue range: `[0.129176024941197550935214713, 0.129176024941197550935214713]`; per-observation stationarity residual `1.797837979320167667871251998e-70`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sged/face_payload.hpp:18` (special difference): `const double m=std::exp(special::lgamma(2/nu)-special::lgamma(1/nu));`. Execution-path causation remains open.

## Case 6124: sged, limit-outlier

Data: `[0.6449948853914544, 0.5314175321796899, 0.10643637622867162, 0.35784486371116986, 1000001.5580291981]`. Holds: `{'mean': -0.032496865700421936, 'xi': 2.5890673965717057}`.
Call: `route_sged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6124` (run through the slot wrapper).
Returned: `{'mean': '-0.032496865700421936', 'sd': '202213.93295473565', 'nu': '1', 'xi': '2.5890673965717057'}`, status `0`, logLik `-70.9797226576888107274498906917870044708251953125`.
Independent logLik at that point: `-70.97972265768880084530368283` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.32e-08`.
Better feasible point: `[-0.03249686570042193550778009126, 202213.928271206386364930063, 1.0, 2.589067396571705703678389909]`; logLik `-70.97972265768879950419387423`.
Stationary local point: `[-0.03249686570042193550778009126, 202213.928271206386364930063, 1.0, 2.589067396571705703678389909]`; logLik `-70.97972265768879950419387423`. Verified coefficient relative error: `2.32e-08`.
Local information eigenvalue range: `[1.000000046322519466431750664, 1.000000046322519466431750664]`; per-observation stationarity residual `1.06000373020651983007099324e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sged/face_payload.hpp:18` (special difference): `const double m=std::exp(special::lgamma(2/nu)-special::lgamma(1/nu));`. Execution-path causation remains open.

## Case 6126: sged, interior

Data: `[0.6449948853914544, 0.5314175321796899, 0.10643637622867162, 0.35784486371116986, 0.4912947510231833, -1.4843850018264122, -0.929136424879163, -0.10582726819365745, -0.8768581195832416, -0.18968909122061467, 1.5580291981210541, -1.2217397805116432]`. Holds: `{'sd': 0.9390299202739361, 'xi': 2.5890673965717057}`.
Call: `route_sged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6126` (run through the slot wrapper).
Returned: `{'mean': '0.1075125137221318', 'sd': '0.93902992027393606', 'nu': '94.474388823425556', 'xi': '2.5890673965717057'}`, status `0`, logLik `-14.1510878196928242545027387677691876888275146484375`.
Independent logLik at that point: `-14.15108781969282557357181773` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.19e-05`.
Better feasible point: `[0.107511942396515378387507657, 0.9390299202739360584502037455, 94.47834305591637680178112212, 2.589067396571705703678389909]`; logLik `-14.15108781965795018522217308`.
Stationary local point: `[0.107511942396515378387507657, 0.9390299202739360584502037455, 94.47834305591637680178112212, 2.589067396571705703678389909]`; logLik `-14.15108781965795018522217308`. Verified coefficient relative error: `4.19e-05`.
Local information eigenvalue range: `[0.002818787237003690950126692096, 10.29591105788476253297276419]`; per-observation stationarity residual `1.778127684292579678472707558e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sged/face_payload.hpp:18` (special difference): `const double m=std::exp(special::lgamma(2/nu)-special::lgamma(1/nu));`. Execution-path causation remains open.

## Case 6139: sged, offset-1e8

Data: `[100000000.64499488, 100000000.53141753, 100000000.10643637, 100000000.35784486, 100000000.49129476, 99999998.515615, 99999999.07086357, 99999999.89417273, 99999999.12314188, 99999999.81031092, 100000001.5580292, 99999998.77826022]`. Holds: `{'sd': 0.9390299202739361, 'xi': 2.5890673965717057}`.
Call: `route_sged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6139` (run through the slot wrapper).
Returned: `{'mean': '100000000.10750853', 'sd': '0.93902992027393606', 'nu': '94.476103894739438', 'xi': '2.5890673965717057'}`, status `0`, logLik `-14.1510878198989331622215104289352893829345703125`.
Independent logLik at that point: `-14.15108781989901202608921295` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.37e-05`.
Better feasible point: `[100000000.1075119461621405455, 0.9390299202739360584502037455, 94.47834547406680855467180593, 2.589067396571705703678389909]`; logLik `-14.15108781981933545446640237`.
Stationary local point: `[100000000.1075119461621405455, 0.9390299202739360584502037455, 94.47834547406680855467180593, 2.589067396571705703678389909]`; logLik `-14.15108781981933545446640237`. Verified coefficient relative error: `2.37e-05`.
Local information eigenvalue range: `[0.002818889340739823450292784457, 10.29591097614822080985646409]`; per-observation stationarity residual `8.043180870748967002281636735e-58`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sged/face_payload.hpp:18` (special difference): `const double m=std::exp(special::lgamma(2/nu)-special::lgamma(1/nu));`. Execution-path causation remains open.

## Case 6142: sged, interior

Data: `[0.6449948853914544, 0.5314175321796899, 0.10643637622867162, 0.35784486371116986, 0.4912947510231833, -1.4843850018264122, -0.929136424879163, -0.10582726819365745, -0.8768581195832416, -0.18968909122061467, 1.5580291981210541, -1.2217397805116432]`. Holds: `{'mean': -0.032496865700421936, 'sd': 0.9390299202739361, 'xi': 2.5890673965717057}`.
Call: `route_sged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6142` (run through the slot wrapper).
Returned: `{'mean': '-0.032496865700421936', 'sd': '0.93902992027393606', 'nu': '842.71428758132174', 'xi': '2.5890673965717057'}`, status `0`, logLik `-14.1544922323005977204957162030041217803955078125`.
Independent logLik at that point: `-14.15449223230061175024155144` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.93e-06`.
Better feasible point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 842.7118213067299689911186316, 2.589067396571705703678389909]`; logLik `-14.15449223230060747111543196`.
Stationary local point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 842.7118213067299689911186316, 2.589067396571705703678389909]`; logLik `-14.15449223230060747111543196`. Verified coefficient relative error: `2.93e-06`.
Local information eigenvalue range: `[0.00008326976938904884627400588142, 0.00008326976938904884627400588142]`; per-observation stationarity residual `2.688243798615588257005422103e-43`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sged/face_payload.hpp:18` (special difference): `const double m=std::exp(special::lgamma(2/nu)-special::lgamma(1/nu));`. Execution-path causation remains open.

## Case 6143: sged, tiny-2

Data: `[0.6449948853914544, 0.5314175321796899]`. Holds: `{'mean': -0.032496865700421936, 'sd': 0.9390299202739361, 'xi': 2.5890673965717057}`.
Call: `route_sged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6143` (run through the slot wrapper).
Returned: `{'mean': '-0.032496865700421936', 'sd': '0.93902992027393606', 'nu': '8.6219144602185125', 'xi': '2.5890673965717057'}`, status `0`, logLik `-2.3143588612492980161050581955350935459136962890625`.
Independent logLik at that point: `-2.314358861249298970225932442` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.08e-07`.
Better feasible point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 8.621913528432503501361909495, 2.589067396571705703678389909]`; logLik `-2.314358861249297757367301604`.
Stationary local point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 8.621913528432503501361909495, 2.589067396571705703678389909]`; logLik `-2.314358861249297757367301604`. Verified coefficient relative error: `1.08e-07`.
Local information eigenvalue range: `[0.1038449356279806207514934422, 0.1038449356279806207514934422]`; per-observation stationarity residual `2.914057573143037212988195798e-54`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sged/face_payload.hpp:18` (special difference): `const double m=std::exp(special::lgamma(2/nu)-special::lgamma(1/nu));`. Execution-path causation remains open.

## Case 6144: sged, tiny-3

Data: `[0.6449948853914544, 0.5314175321796899, 0.10643637622867162]`. Holds: `{'mean': -0.032496865700421936, 'sd': 0.9390299202739361, 'xi': 2.5890673965717057}`.
Call: `route_sged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6144` (run through the slot wrapper).
Returned: `{'mean': '-0.032496865700421936', 'sd': '0.93902992027393606', 'nu': '6.4402152071250214', 'xi': '2.5890673965717057'}`, status `0`, logLik `-3.444000671444798822307120644836686551570892333984375`.
Independent logLik at that point: `-3.444000671444800271673837475` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.23e-08`.
Better feasible point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 6.440215063552139685650190982, 2.589067396571705703678389909]`; logLik `-3.444000671444800194767975079`.
Stationary local point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 6.440215063552139685650190982, 2.589067396571705703678389909]`; logLik `-3.444000671444800194767975079`. Verified coefficient relative error: `2.23e-08`.
Local information eigenvalue range: `[0.1031630393898607088113635231, 0.1031630393898607088113635231]`; per-observation stationarity residual `3.450723323330935564583008363e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sged/face_payload.hpp:18` (special difference): `const double m=std::exp(special::lgamma(2/nu)-special::lgamma(1/nu));`. Execution-path causation remains open.

## Case 6145: sged, tiny-5

Data: `[0.6449948853914544, 0.5314175321796899, 0.10643637622867162, 0.35784486371116986, 0.4912947510231833]`. Holds: `{'mean': -0.032496865700421936, 'sd': 0.9390299202739361, 'xi': 2.5890673965717057}`.
Call: `route_sged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6145` (run through the slot wrapper).
Returned: `{'mean': '-0.032496865700421936', 'sd': '0.93902992027393606', 'nu': '6.1018836167212136', 'xi': '2.5890673965717057'}`, status `0`, logLik `-5.7224086539399312556497534387744963169097900390625`.
Independent logLik at that point: `-5.72240865393993407795851739` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.01e-08`.
Better feasible point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 6.10188373956434421966903389, 2.589067396571705703678389909]`; logLik `-5.722408653939933961111430156`.
Stationary local point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 6.10188373956434421966903389, 2.589067396571705703678389909]`; logLik `-5.722408653939933961111430156`. Verified coefficient relative error: `2.01e-08`.
Local information eigenvalue range: `[0.1153198200910880050957131268, 0.1153198200910880050957131268]`; per-observation stationarity residual `1.614267119645544389704942993e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sged/face_payload.hpp:18` (special difference): `const double m=std::exp(special::lgamma(2/nu)-special::lgamma(1/nu));`. Execution-path causation remains open.

## Case 6146: sged, ties

Data: `[0.6449948853914544, 0.6449948853914544, 0.6449948853914544, 0.6449948853914544, 0.5314175321796899, 0.10643637622867162]`. Holds: `{'mean': -0.032496865700421936, 'sd': 0.9390299202739361, 'xi': 2.5890673965717057}`.
Call: `route_sged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6146` (run through the slot wrapper).
Returned: `{'mean': '-0.032496865700421936', 'sd': '0.93902992027393606', 'nu': '8.3145074252275144', 'xi': '2.5890673965717057'}`, status `0`, logLik `-6.94022766650029065971239106147550046443939208984375`.
Independent logLik at that point: `-6.94022766650029330841600272` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.88e-08`.
Better feasible point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 8.314507102558781264628848408, 2.589067396571705703678389909]`; logLik `-6.940227666500292872135612157`.
Stationary local point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 8.314507102558781264628848408, 2.589067396571705703678389909]`; logLik `-6.940227666500292872135612157`. Verified coefficient relative error: `3.88e-08`.
Local information eigenvalue range: `[0.09656144412937279771706012322, 0.09656144412937279771706012322]`; per-observation stationarity residual `5.580250721319516834303699238e-58`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sged/face_payload.hpp:18` (special difference): `const double m=std::exp(special::lgamma(2/nu)-special::lgamma(1/nu));`. Execution-path causation remains open.

## Case 6147: sged, constant-0.106436-0.0001

Data: `[0.10641508895342588, 0.10642573259104876, 0.10643637622867162, 0.10644701986629448, 0.10646830714154022]`. Holds: `{'mean': -0.032496865700421936, 'sd': 0.9390299202739361, 'xi': 2.5890673965717057}`.
Call: `route_sged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6147` (run through the slot wrapper).
Returned: `{'mean': '-0.032496865700421936', 'sd': '0.93902992027393606', 'nu': '1.9024569204324218', 'xi': '2.5890673965717057'}`, status `0`, logLik `-5.221458125065669975128912483341991901397705078125`.
Independent logLik at that point: `-5.221458125065671712483771324` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.02e-08`.
Better feasible point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 1.902456862989692899612861345, 2.589067396571705703678389909]`; logLik `-5.221458125065671438339745295`.
Stationary local point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 1.902456862989692899612861345, 2.589067396571705703678389909]`; logLik `-5.221458125065671438339745295`. Verified coefficient relative error: `3.02e-08`.
Local information eigenvalue range: `[0.1202813586196891805432254917, 0.1202813586196891805432254917]`; per-observation stationarity residual `4.127828994946057668480260085e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sged/face_payload.hpp:18` (special difference): `const double m=std::exp(special::lgamma(2/nu)-special::lgamma(1/nu));`. Execution-path causation remains open.

## Case 6148: sged, constant-0.106436-1e-08

Data: `[0.1064363740999441, 0.10643637516430786, 0.10643637622867162, 0.10643637729303539, 0.10643637942176291]`. Holds: `{'mean': -0.032496865700421936, 'sd': 0.9390299202739361, 'xi': 2.5890673965717057}`.
Call: `route_sged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6148` (run through the slot wrapper).
Returned: `{'mean': '-0.032496865700421936', 'sd': '0.93902992027393606', 'nu': '1.9024334613701686', 'xi': '2.5890673965717057'}`, status `0`, logLik `-5.2214521274078560253428804571740329265594482421875`.
Independent logLik at that point: `-5.221452127407858443542306027` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.33e-08`.
Better feasible point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 1.902433435984666476204846018, 2.589067396571705703678389909]`; logLik `-5.221452127407858390001378812`.
Stationary local point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 1.902433435984666476204846018, 2.589067396571705703678389909]`; logLik `-5.221452127407858390001378812`. Verified coefficient relative error: `1.33e-08`.
Local information eigenvalue range: `[0.1202799728685629687287830824, 0.1202799728685629687287830824]`; per-observation stationarity residual `6.005262007261354544393945819e-64`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sged/face_payload.hpp:18` (special difference): `const double m=std::exp(special::lgamma(2/nu)-special::lgamma(1/nu));`. Execution-path causation remains open.

## Case 6149: sged, constant-0.106436-1e-12

Data: `[0.10643637622845875, 0.10643637622856518, 0.10643637622867162, 0.10643637622877807, 0.10643637622899094]`. Holds: `{'mean': -0.032496865700421936, 'sd': 0.9390299202739361, 'xi': 2.5890673965717057}`.
Call: `route_sged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6149` (run through the slot wrapper).
Returned: `{'mean': '-0.032496865700421936', 'sd': '0.93902992027393606', 'nu': '1.9024334613701686', 'xi': '2.5890673965717057'}`, status `0`, logLik `-5.22145212680812420558140729553997516632080078125`.
Independent logLik at that point: `-5.221452126808126702992131851` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.46e-08`.
Better feasible point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 1.902433433641880895649781136, 2.589067396571705703678389909]`; logLik `-5.221452126808126639112783806`.
Stationary local point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 1.902433433641880895649781136, 2.589067396571705703678389909]`; logLik `-5.221452126808126639112783806`. Verified coefficient relative error: `1.46e-08`.
Local information eigenvalue range: `[0.1202799730266098681957179636, 0.1202799730266098681957179636]`; per-observation stationarity residual `1.216831180645657391252355443e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sged/face_payload.hpp:18` (special difference): `const double m=std::exp(special::lgamma(2/nu)-special::lgamma(1/nu));`. Execution-path causation remains open.

## Case 6150: sged, constant-1-0.0001

Data: `[0.9998, 0.9999, 1.0, 1.0001, 1.0003]`. Holds: `{'mean': -0.032496865700421936, 'sd': 0.9390299202739361, 'xi': 2.5890673965717057}`.
Call: `route_sged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6150` (run through the slot wrapper).
Returned: `{'mean': '-0.032496865700421936', 'sd': '0.93902992027393606', 'nu': '24.961836388683409', 'xi': '2.5890673965717057'}`, status `0`, logLik `-5.87862127083340180178083755890838801860809326171875`.
Independent logLik at that point: `-5.878621270833404545804870261` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.03e-08`.
Better feasible point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 24.96183588139793679373950384, 2.589067396571705703678389909]`; logLik `-5.878621270833404510679192676`.
Stationary local point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 24.96183588139793679373950384, 2.589067396571705703678389909]`; logLik `-5.878621270833404510679192676`. Verified coefficient relative error: `2.03e-08`.
Local information eigenvalue range: `[0.03401989278717674494080457481, 0.03401989278717674494080457481]`; per-observation stationarity residual `1.844817173211183195097222233e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sged/face_payload.hpp:18` (special difference): `const double m=std::exp(special::lgamma(2/nu)-special::lgamma(1/nu));`. Execution-path causation remains open.

## Case 6151: sged, constant-1-1e-08

Data: `[0.99999998, 0.99999999, 1.0, 1.00000001, 1.00000003]`. Holds: `{'mean': -0.032496865700421936, 'sd': 0.9390299202739361, 'xi': 2.5890673965717057}`.
Call: `route_sged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6151` (run through the slot wrapper).
Returned: `{'mean': '-0.032496865700421936', 'sd': '0.93902992027393606', 'nu': '24.960355828109776', 'xi': '2.5890673965717057'}`, status `0`, logLik `-5.8786192532656986742267690715380012989044189453125`.
Independent logLik at that point: `-5.878619253265704313722891004` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.8e-08`.
Better feasible point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 24.96035677676627641250546236, 2.589067396571705703678389909]`; logLik `-5.878619253265704190859371665`.
Stationary local point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 24.96035677676627641250546236, 2.589067396571705703678389909]`; logLik `-5.878619253265704190859371665`. Verified coefficient relative error: `3.8e-08`.
Local information eigenvalue range: `[0.03402251924758993031363054867, 0.03402251924758993031363054867]`; per-observation stationarity residual `2.760536051572202622285375376e-57`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sged/face_payload.hpp:18` (special difference): `const double m=std::exp(special::lgamma(2/nu)-special::lgamma(1/nu));`. Execution-path causation remains open.

## Case 6152: sged, constant-1-1e-12

Data: `[0.999999999998, 0.999999999999, 1.0, 1.000000000001, 1.000000000003]`. Holds: `{'mean': -0.032496865700421936, 'sd': 0.9390299202739361, 'xi': 2.5890673965717057}`.
Call: `route_sged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6152` (run through the slot wrapper).
Returned: `{'mean': '-0.032496865700421936', 'sd': '0.93902992027393606', 'nu': '24.960356150382506', 'xi': '2.5890673965717057'}`, status `0`, logLik `-5.87861925306552546288685334729962050914764404296875`.
Independent logLik at that point: `-5.878619253065529269344877856` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.92e-08`.
Better feasible point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 24.96035662978674664814893255, 2.589067396571705703678389909]`; logLik `-5.878619253065529237968073684`.
Stationary local point: `[-0.03249686570042193550778009126, 0.9390299202739360584502037455, 24.96035662978674664814893255, 2.589067396571705703678389909]`; logLik `-5.878619253065529237968073684`. Verified coefficient relative error: `1.92e-08`.
Local information eigenvalue range: `[0.03402252078654152747604372337, 0.03402252078654152747604372337]`; per-observation stationarity residual `1.17418532308321735441572874e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sged/face_payload.hpp:18` (special difference): `const double m=std::exp(special::lgamma(2/nu)-special::lgamma(1/nu));`. Execution-path causation remains open.

## Case 6222: sgomp, interior

Data: `[3.5336709827544537, 1.5849655909937659, 0.806704800057856, 5.41617723251611, 6.215744211105518, 1.1251403755435831, 2.00830374390256, 1.7288130863551756, 2.610115059971265, 1.738347077247835, 2.2037163602736096, 0.4447676691549313]`. Holds: `{}`.
Call: `route_sgomp(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6222` (run through the slot wrapper).
Returned: `{'b': '0.77563829015735752', 'eta': '2.4591091949445945'}`, status `0`, logLik `-21.197743208299538508754267240874469280242919921875`.
Independent logLik at that point: `-21.19774320829954152679498878` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.72e-08`.
Better feasible point: `[0.7756383015976942459794394742, 2.459109261947435723841484975]`; logLik `-21.19774320829954030689821465`.
Stationary local point: `[0.7756383015976942459794394742, 2.459109261947435723841484975]`; logLik `-21.19774320829954030689821465`. Verified coefficient relative error: `2.72e-08`.
Local information eigenvalue range: `[0.1471190432577541121942645676, 2.581842026147117864320177128]`; per-observation stationarity residual `1.736856768465433476054988001e-64`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sgomp/mle.hpp:16` (step stop): `double delta=x.size()*(std::abs(relative)<0.5 ? std::log1p(relative) : std::log(a.b)-std::log(b.b));`. Execution-path causation remains open.

## Case 6223: sgomp, tiny-2

Data: `[3.5336709827544537, 1.5849655909937659]`. Holds: `{}`.
Call: `route_sgomp(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6223` (run through the slot wrapper).
Returned: `{'b': '1.2239715990552404', 'eta': '11.653346303457068'}`, status `0`, logLik `-2.7681606976691828236880610347725450992584228515625`.
Independent logLik at that point: `-2.768160697669183973347126909` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.55e-08`.
Better feasible point: `[1.223971585845288550640734974, 11.65334600683520380560221363]`; logLik `-2.768160697669183812147290185`.
Stationary local point: `[1.223971585845288550640734974, 11.65334600683520380560221363]`; logLik `-2.768160697669183812147290185`. Verified coefficient relative error: `2.55e-08`.
Local information eigenvalue range: `[0.1726052642314723684819816595, 6.704866509408059869313449145]`; per-observation stationarity residual `2.580240325130546127518508442e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sgomp/mle.hpp:16` (step stop): `double delta=x.size()*(std::abs(relative)<0.5 ? std::log1p(relative) : std::log(a.b)-std::log(b.b));`. Execution-path causation remains open.

## Case 6224: sgomp, tiny-3

Data: `[3.5336709827544537, 1.5849655909937659, 0.806704800057856]`. Holds: `{}`.
Call: `route_sgomp(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6224` (run through the slot wrapper).
Returned: `{'b': '1.0938288031429724', 'eta': '3.6094100437416583'}`, status `0`, logLik `-4.39168266252448091790938633494079113006591796875`.
Independent logLik at that point: `-4.391682662524481318474288897` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.23e-08`.
Better feasible point: `[1.093828810650775164441303145, 3.609410088294383961587722482]`; logLik `-4.391682662524481241144045877`.
Stationary local point: `[1.093828810650775164441303145, 3.609410088294383961587722482]`; logLik `-4.391682662524481241144045877`. Verified coefficient relative error: `1.23e-08`.
Local information eigenvalue range: `[0.1846808173785226269129856524, 3.21639057534674882658921869]`; per-observation stationarity residual `1.066555258148840486723112009e-65`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sgomp/mle.hpp:16` (step stop): `double delta=x.size()*(std::abs(relative)<0.5 ? std::log1p(relative) : std::log(a.b)-std::log(b.b));`. Execution-path causation remains open.

## Case 6234: sgomp, lower-1e-12

Data: `[1e-12, 2e-12, 3e-12, 5e-12, 9e-12]`. Holds: `{}`.
Call: `route_sgomp(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6234` (run through the slot wrapper).
Returned: `{'b': '430394784809.93024', 'eta': '1.8494857555430055'}`, status `0`, logLik `126.663344881478423076259787194430828094482421875`.
Independent logLik at that point: `126.6633448814784181531250256` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.22e-08`.
Better feasible point: `[430394794150.7954622192936271, 1.849485833637217437825500271]`; logLik `126.6633448814784191048900897`.
Stationary local point: `[430394794150.7954622192936271, 1.849485833637217437825500271]`; logLik `126.6633448814784191048900897`. Verified coefficient relative error: `4.22e-08`.
Local information eigenvalue range: `[0.1151381637854115147658602284, 2.174736789155828319710071791]`; per-observation stationarity residual `2.149364101038566753567452439e-64`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sgomp/mle.hpp:16` (step stop): `double delta=x.size()*(std::abs(relative)<0.5 ? std::log1p(relative) : std::log(a.b)-std::log(b.b));`. Execution-path causation remains open.

## Case 6237: sgomp, scale-1e-150

Data: `[3.533670982754454e-150, 1.5849655909937658e-150, 8.06704800057856e-151, 5.41617723251611e-150, 6.215744211105518e-150, 1.1251403755435832e-150, 2.00830374390256e-150, 1.7288130863551756e-150, 2.610115059971265e-150, 1.7383470772478352e-150, 2.2037163602736095e-150, 4.447676691549313e-151]`. Holds: `{}`.
Call: `route_sgomp(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6237` (run through the slot wrapper).
Returned: `{'b': '7.756382908359077e+149', 'eta': '2.4591091988458254'}`, status `0`, logLik `4123.45542418098193593323230743408203125`.
Independent logLik at that point: `4123.45542418098268949290788` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.57e-08`.
Better feasible point: `[7.756383015976942151099136945e+149, 2.45910926194743550131044808]`; logLik `4123.455424180982690572387698`.
Stationary local point: `[7.756383015976942151099136945e+149, 2.45910926194743550131044808]`; logLik `4123.455424180982690572387698`. Verified coefficient relative error: `2.57e-08`.
Local information eigenvalue range: `[0.1471190437022449238050985376, 2.58184203105581252609486729]`; per-observation stationarity residual `1.100066755510206718599261197e-64`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sgomp/mle.hpp:16` (step stop): `double delta=x.size()*(std::abs(relative)<0.5 ? std::log1p(relative) : std::log(a.b)-std::log(b.b));`. Execution-path causation remains open.

## Case 6238: sgomp, scale-1e+150

Data: `[3.5336709827544536e+150, 1.5849655909937658e+150, 8.06704800057856e+149, 5.4161772325161095e+150, 6.2157442111055176e+150, 1.125140375543583e+150, 2.00830374390256e+150, 1.7288130863551754e+150, 2.610115059971265e+150, 1.738347077247835e+150, 2.2037163602736097e+150, 4.4476766915493135e+149]`. Holds: `{}`.
Call: `route_sgomp(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6238` (run through the slot wrapper).
Returned: `{'b': '7.7563829009617262e-151', 'eta': '2.4591091945342911'}`, status `0`, logLik `-4165.8509105975817874423228204250335693359375`.
Independent logLik at that point: `-4165.850910597581772370009457` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.74e-08`.
Better feasible point: `[7.756383015976942720232223879e-151, 2.459109261947435776445746761]`; logLik `-4165.850910597581771137029293`.
Stationary local point: `[7.756383015976942720232223879e-151, 2.459109261947435776445746761]`; logLik `-4165.850910597581771137029293`. Verified coefficient relative error: `2.74e-08`.
Local information eigenvalue range: `[0.1471190432114170756716890323, 2.58184202569141088290310631]`; per-observation stationarity residual `1.852297521548093259665179942e-64`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sgomp/mle.hpp:16` (step stop): `double delta=x.size()*(std::abs(relative)<0.5 ? std::log1p(relative) : std::log(a.b)-std::log(b.b));`. Execution-path causation remains open.

## Case 6298: skellam, interior

Data: `[2, 3, -4, -2, -5, 3, -3, 1, -1, 3, -1, 0]`. Holds: `{}`.
Call: `route_skellam(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6298` (run through the slot wrapper).
Returned: `{'mu1': '3.5859645278464263', 'mu2': '3.9192978611797598'}`, status `0`, logLik `-28.969424563990823884296332835219800472259521484375`.
Independent logLik at that point: `-28.96942456399082873587753569` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.83e-08`.
Better feasible point: `[3.585964462090268442818372583, 3.919297795423601776151705917]`; logLik `-28.96942456399082781021686824`.
Stationary local point: `[3.585964462090268442818372583, 3.919297795423601776151705917]`; logLik `-28.96942456399082781021686824`. Verified coefficient relative error: `1.83e-08`.
Local information eigenvalue range: `[0.2497110162779011379604880389, 3.760561152117022370672707564]`; per-observation stationarity residual `2.381383500688896471234968585e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/skellam/mle.hpp:109` (step stop): `for(;it<maxit && hi-lo>std::min(tol,1e-8);++it) {`. Execution-path causation remains open.

## Case 6300: skellam, tiny-3

Data: `[2, 3, -4]`. Holds: `{}`.
Call: `route_skellam(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6300` (run through the slot wrapper).
Returned: `{'mu1': '5.1764904871263582', 'mu2': '4.8431571537930251'}`, status `0`, logLik `-7.666675848364917555954889394342899322509765625`.
Independent logLik at that point: `-7.666675848364918232915878113` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.71e-08`.
Better feasible point: `[5.17649030764685367736790214, 4.843156974313520344034568806]`; logLik `-7.666675848364917230797867556`.
Stationary local point: `[5.17649030764685367736790214, 4.843156974313520344034568806]`; logLik `-7.666675848364917230797867556`. Verified coefficient relative error: `3.71e-08`.
Local information eigenvalue range: `[0.2593847013909631257730294957, 5.015671412761273733069027122]`; per-observation stationarity residual `9.399929122797855347371900141e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/skellam/mle.hpp:109` (step stop): `for(;it<maxit && hi-lo>std::min(tol,1e-8);++it) {`. Execution-path causation remains open.

## Case 6301: skellam, tiny-5

Data: `[2, 3, -4, -2, -5]`. Holds: `{}`.
Call: `route_skellam(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6301` (run through the slot wrapper).
Returned: `{'mu1': '4.7261094839653053', 'mu2': '5.9261094839653055'}`, status `0`, logLik `-12.9313177396870937485573449521325528621673583984375`.
Independent logLik at that point: `-12.93131773968709465099552648` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.15e-08`.
Better feasible point: `[4.726109429769127549924065187, 5.926109429769127549924065187]`; logLik `-12.93131773968709451591501953`.
Stationary local point: `[4.726109429769127549924065187, 5.926109429769127549924065187]`; logLik `-12.93131773968709451591501953`. Verified coefficient relative error: `1.15e-08`.
Local information eigenvalue range: `[0.2509956685201226447497401076, 5.397043908545865176156559679]`; per-observation stationarity residual `4.315036363345717274769373096e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/skellam/mle.hpp:109` (step stop): `for(;it<maxit && hi-lo>std::min(tol,1e-8);++it) {`. Execution-path causation remains open.

## Case 6302: skellam, ties

Data: `[2, 2, 2, 2, 3, -4]`. Holds: `{}`.
Call: `route_skellam(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6302` (run through the slot wrapper).
Returned: `{'mu1': '3.6170761596822016', 'mu2': '2.4504094930155347'}`, status `0`, logLik `-13.721239452327377961182719445787370204925537109375`.
Independent logLik at that point: `-13.72123945232737873779781311` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.1e-08`.
Better feasible point: `[3.617076083749944159440094009, 2.450409417083277492773427343]`; logLik `-13.72123945232737769162524805`.
Stationary local point: `[3.617076083749944159440094009, 2.450409417083277492773427343]`; logLik `-13.72123945232737769162524805`. Verified coefficient relative error: `3.1e-08`.
Local information eigenvalue range: `[0.2481363061638637124300407737, 3.155898627278652091261831085]`; per-observation stationarity residual `7.60105102357583896668594461e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/skellam/mle.hpp:109` (step stop): `for(;it<maxit && hi-lo>std::min(tol,1e-8);++it) {`. Execution-path causation remains open.

## Case 6354: slash, interior

Data: `[-6.860963357530646, 2.731907054614555, -1.2506362937344324, 0.03426346894790194, -5.436914450025588, 0.7185865176878535, -4.559678851396354, 0.21037326050831645, 1.8512830440002235, 1.9681580392654994, -1.1952891020578855, 1.62584842171206]`. Holds: `{}`.
Call: `route_slash(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6354` (run through the slot wrapper).
Returned: `{'mu': '0.39605972403022272', 'sigma': '1.0397687569314427'}`, status `0`, logLik `-31.651450422736701284520677290856838226318359375`.
Independent logLik at that point: `-31.65145042273670385100106318` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.24e-08`.
Better feasible point: `[0.3960596716453139558802112429, 1.039768765322840904488577075]`; logLik `-31.65145042273670093591529563`.
Stationary local point: `[0.3960596716453139558802112429, 1.039768765322840904488577075]`; logLik `-31.65145042273670093591529563`. Verified coefficient relative error: `5.24e-08`.
Local information eigenvalue range: `[0.6537006758053859147825343873, 17.92928981866559466776749416]`; per-observation stationarity residual `9.520458971500224308371042008e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/slash/mle.hpp:31` (step stop): `CPPUNIV_EXTENDED_REAL radius=0;for(double v:x)radius=std::max(radius,std::abs(static_cast<CPPUNIV_EXTENDED_REAL>(v)-m));`. Execution-path causation remains open.

## Case 6355: slash, tiny-2

Data: `[-6.860963357530646, 2.731907054614555]`. Holds: `{}`.
Call: `route_slash(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6355` (run through the slot wrapper).
Returned: `{'mu': '-2.0645281917398943', 'sigma': '3.0257582531543168'}`, status `0`, logLik `-6.565062928205801284775589010678231716156005859375`.
Independent logLik at that point: `-6.565062928205802095871143286` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.95e-08`.
Better feasible point: `[-2.064528151458045357458104263, 3.025758253154316894014830478]`; logLik `-6.565062928205802059698311864`.
Stationary local point: `[-2.064528151458045357458104263, 3.025758253154316894014830478]`; logLik `-6.565062928205802059698311864`. Verified coefficient relative error: `1.95e-08`.
Local information eigenvalue range: `[1.512862417252339250252771911, 2.051449669009357415861900933]`; per-observation stationarity residual `2.166509522691253460553552984e-68`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/slash/mle.hpp:31` (step stop): `CPPUNIV_EXTENDED_REAL radius=0;for(double v:x)radius=std::max(radius,std::abs(static_cast<CPPUNIV_EXTENDED_REAL>(v)-m));`. Execution-path causation remains open.

## Case 6357: slash, tiny-5

Data: `[-6.860963357530646, 2.731907054614555, -1.2506362937344324, 0.03426346894790194, -5.436914450025588]`. Holds: `{}`.
Call: `route_slash(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6357` (run through the slot wrapper).
Returned: `{'mu': '-2.0022750005761019', 'sigma': '1.9927809318242165'}`, status `0`, logLik `-14.68617878059104242538523976691067218780517578125`.
Independent logLik at that point: `-14.68617878059104458062275411` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.72e-08`.
Better feasible point: `[-2.002274966235169871324957231, 1.992780929752406637606268597]`; logLik `-14.68617878059104444683341818`.
Stationary local point: `[-2.002274966235169871324957231, 1.992780929752406637606268597]`; logLik `-14.68617878059104444683341818`. Verified coefficient relative error: `1.72e-08`.
Local information eigenvalue range: `[1.049306830080618748063523819, 4.297590003210058295817432764]`; per-observation stationarity residual `1.332110274061932312793594805e-68`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/slash/mle.hpp:31` (step stop): `CPPUNIV_EXTENDED_REAL radius=0;for(double v:x)radius=std::max(radius,std::abs(static_cast<CPPUNIV_EXTENDED_REAL>(v)-m));`. Execution-path causation remains open.

## Case 6366: slash, scale-1e+150

Data: `[-6.860963357530646e+150, 2.731907054614555e+150, -1.2506362937344323e+150, 3.426346894790194e+148, -5.436914450025588e+150, 7.185865176878535e+149, -4.559678851396354e+150, 2.1037326050831643e+149, 1.8512830440002233e+150, 1.9681580392654993e+150, -1.1952891020578856e+150, 1.6258484217120599e+150]`. Holds: `{}`.
Call: `route_slash(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6366` (run through the slot wrapper).
Returned: `{'mu': '3.9605964748386062e+149', 'sigma': '1.0397687691931994e+150'}`, status `0`, logLik `-4176.304617812018477707169950008392333984375`.
Independent logLik at that point: `-4176.304617812018932094539778` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.1e-08`.
Better feasible point: `[3.960596716453139221338390125e+149, 1.039768765322840853653061274e+150]`; logLik `-4176.304617812018931474406478`.
Stationary local point: `[3.960596716453139221338390125e+149, 1.039768765322840853653061274e+150]`; logLik `-4176.304617812018931474406478`. Verified coefficient relative error: `6.1e-08`.
Local information eigenvalue range: `[0.653700691164658305387310667, 17.92928982027324588078020267]`; per-observation stationarity residual `1.949812849665275807400109797e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/slash/mle.hpp:31` (step stop): `CPPUNIV_EXTENDED_REAL radius=0;for(double v:x)radius=std::max(radius,std::abs(static_cast<CPPUNIV_EXTENDED_REAL>(v)-m));`. Execution-path causation remains open.

## Case 6368: slash, limit-outlier

Data: `[-6.860963357530646, 2.731907054614555, -1.2506362937344324, 0.03426346894790194, 1000002.7319070547]`. Holds: `{}`.
Call: `route_slash(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6368` (run through the slot wrapper).
Returned: `{'mu': '-0.61183273239659641', 'sigma': '2.0936204863099293'}`, status `0`, logLik `-39.33834284500141365015224437229335308074951171875`.
Independent logLik at that point: `-39.33834284500141729869029716` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.53e-07`.
Better feasible point: `[-0.6118328850394561002600439918, 2.093620530686129227999928096]`; logLik `-39.33834284500141506617141015`.
Stationary local point: `[-0.6118328850394561002600439918, 2.093620530686129227999928096]`; logLik `-39.33834284500141506617141015`. Verified coefficient relative error: `1.53e-07`.
Local information eigenvalue range: `[0.3920981775198666843750276439, 47745977495.20634619494648523]`; per-observation stationarity residual `8.176044962926119957320760723e-55`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/slash/mle.hpp:31` (step stop): `CPPUNIV_EXTENDED_REAL radius=0;for(double v:x)radius=std::max(radius,std::abs(static_cast<CPPUNIV_EXTENDED_REAL>(v)-m));`. Execution-path causation remains open.

## Case 6386: slash, interior

Data: `[-6.860963357530646, 2.731907054614555, -1.2506362937344324, 0.03426346894790194, -5.436914450025588, 0.7185865176878535, -4.559678851396354, 0.21037326050831645, 1.8512830440002235, 1.9681580392654994, -1.1952891020578855, 1.62584842171206]`. Holds: `{'sigma': 1.1437689913361664}`.
Call: `route_slash(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6386` (run through the slot wrapper).
Returned: `{'mu': '0.33671136635261323', 'sigma': '1.1437689913361664'}`, status `0`, logLik `-31.68611762735903170096207759343087673187255859375`.
Independent logLik at that point: `-31.68611762735903664670479163` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.64e-08`.
Better feasible point: `[0.336711319903331876747250825, 1.143768991336166385153205738]`; logLik `-31.68611762735903440817776856`.
Stationary local point: `[0.336711319903331876747250825, 1.143768991336166385153205738]`; logLik `-31.68611762735903440817776856`. Verified coefficient relative error: `4.64e-08`.
Local information eigenvalue range: `[15.91293346424496208700452046, 15.91293346424496208700452046]`; per-observation stationarity residual `5.114152064351601412256034031e-66`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/slash/mle.hpp:31` (step stop): `CPPUNIV_EXTENDED_REAL radius=0;for(double v:x)radius=std::max(radius,std::abs(static_cast<CPPUNIV_EXTENDED_REAL>(v)-m));`. Execution-path causation remains open.

## Case 6389: slash, tiny-5

Data: `[-6.860963357530646, 2.731907054614555, -1.2506362937344324, 0.03426346894790194, -5.436914450025588]`. Holds: `{'sigma': 1.1437689913361664}`.
Call: `route_slash(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6389` (run through the slot wrapper).
Returned: `{'mu': '-0.98862787869443447', 'sigma': '1.1437689913361664'}`, status `0`, logLik `-15.2118948550115096196577724185772240161895751953125`.
Independent logLik at that point: `-15.21189485501151163746291821` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.39e-08`.
Better feasible point: `[-0.9886278926060381447513484046, 1.143768991336166385153205738]`; logLik `-15.21189485501151159654674099`.
Stationary local point: `[-0.9886278926060381447513484046, 1.143768991336166385153205738]`; logLik `-15.21189485501151159654674099`. Verified coefficient relative error: `1.39e-08`.
Local information eigenvalue range: `[7.782117855931311973602121001, 7.782117855931311973602121001]`; per-observation stationarity residual `1.193559612733174057296197166e-68`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/slash/mle.hpp:31` (step stop): `CPPUNIV_EXTENDED_REAL radius=0;for(double v:x)radius=std::max(radius,std::abs(static_cast<CPPUNIV_EXTENDED_REAL>(v)-m));`. Execution-path causation remains open.

## Case 6394: slash, constant-1-0.0001

Data: `[0.9998, 0.9999, 1.0, 1.0001, 1.0003]`. Holds: `{'sigma': 1.1437689913361664}`.
Call: `route_slash(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6394` (run through the slot wrapper).
Returned: `{'mu': '1.0000200140054887', 'sigma': '1.1437689913361664'}`, status `0`, logLik `-8.7320733066893776452843667357228696346282958984375`.
Independent logLik at that point: `-8.732073306689380983628737651` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.4e-08`.
Better feasible point: `[1.00001999999987157780260068, 1.143768991336166385153205738]`; logLik `-8.732073306689380796199269841`.
Stationary local point: `[1.00001999999987157780260068, 1.143768991336166385153205738]`; logLik `-8.732073306689380796199269841`. Verified coefficient relative error: `1.4e-08`.
Local information eigenvalue range: `[0.00000009555059022620054701337608905, 0.00000009555059022620054701337608905]`; per-observation stationarity residual `5.775945731053943453194473219e-67`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/slash/mle.hpp:31` (step stop): `CPPUNIV_EXTENDED_REAL radius=0;for(double v:x)radius=std::max(radius,std::abs(static_cast<CPPUNIV_EXTENDED_REAL>(v)-m));`. Execution-path causation remains open.

## Case 6400: slash, limit-outlier

Data: `[-6.860963357530646, 2.731907054614555, -1.2506362937344324, 0.03426346894790194, 1000002.7319070547]`. Holds: `{'sigma': 1.1437689913361664}`.
Call: `route_slash(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6400` (run through the slot wrapper).
Returned: `{'mu': '-0.18162939492709543', 'sigma': '1.1437689913361664'}`, status `0`, logLik `-39.673573006502380167148658074438571929931640625`.
Independent logLik at that point: `-39.67357300650238200765268232` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.45e-08`.
Better feasible point: `[-0.181629449379642390913681196, 1.143768991336166385153205738]`; logLik `-39.67357300650238115354318326`.
Stationary local point: `[-0.181629449379642390913681196, 1.143768991336166385153205738]`; logLik `-39.67357300650238115354318326`. Verified coefficient relative error: `5.45e-08`.
Local information eigenvalue range: `[115224671520.2002666552365808, 115224671520.2002666552365808]`; per-observation stationarity residual `3.234647937074553474363559718e-64`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/slash/mle.hpp:31` (step stop): `CPPUNIV_EXTENDED_REAL radius=0;for(double v:x)radius=std::max(radius,std::abs(static_cast<CPPUNIV_EXTENDED_REAL>(v)-m));`. Execution-path causation remains open.

## Case 6599: snorm, constant--0.140046-0.0001

Data: `[-0.1400741601380333, -0.14006015552294254, -0.14004615090785175, -0.14003214629276095, -0.1400041370625794]`. Holds: `{'mean': -0.020798452221064478, 'sd': 0.9992068963337329}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6599` (run through the slot wrapper).
Returned: `{'mean': '-0.020798452221064478', 'sd': '0.99920689633373294', 'xi': '1.1008752246047069'}`, status `0`, logLik `-4.5826484352083252105103383655659854412078857421875`.
Independent logLik at that point: `-4.582648435208326482368472989` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.52e-08`.
Better feasible point: `[-0.02079845222106447774623738667, 0.9992068963337329412510712245, 1.100875207899018156565101704]`; logLik `-4.582648435208325635280980328`.
Stationary local point: `[-0.02079845222106447774623738667, 0.9992068963337329412510712245, 1.100875207899018156565101704]`; logLik `-4.582648435208325635280980328`. Verified coefficient relative error: `1.52e-08`.
Local information eigenvalue range: `[1.471416800435111964783923569, 1.471416800435111964783923569]`; per-observation stationarity residual `5.642082039537112759641374586e-58`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6600: snorm, constant--0.140046-1e-08

Data: `[-0.14004615370877477, -0.14004615230831324, -0.14004615090785175, -0.14004614950739025, -0.14004614670646723]`. Holds: `{'mean': -0.020798452221064478, 'sd': 0.9992068963337329}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6600` (run through the slot wrapper).
Returned: `{'mean': '-0.020798452221064478', 'sd': '0.99920689633373294', 'xi': '1.1008778474165559'}`, status `0`, logLik `-4.58264804449381824014153608004562556743621826171875`.
Independent logLik at that point: `-4.582648044493819487614372847` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.39e-08`.
Better feasible point: `[-0.02079845222106447774623738667, 0.9992068963337329412510712245, 1.100877862670794879854014084]`; logLik `-4.58264804449381878133861201`.
Stationary local point: `[-0.02079845222106447774623738667, 0.9992068963337329412510712245, 1.100877862670794879854014084]`; logLik `-4.58264804449381878133861201`. Verified coefficient relative error: `1.39e-08`.
Local information eigenvalue range: `[1.471402351827907965950073966, 1.471402351827907965950073966]`; per-observation stationarity residual `2.726715746891877814908886982e-58`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6601: snorm, constant--0.140046-1e-12

Data: `[-0.14004615090813183, -0.1400461509079918, -0.14004615090785175, -0.1400461509077117, -0.1400461509074316]`. Holds: `{'mean': -0.020798452221064478, 'sd': 0.9992068963337329}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6601` (run through the slot wrapper).
Returned: `{'mean': '-0.020798452221064478', 'sd': '0.99920689633373294', 'xi': '1.1008778511700499'}`, status `0`, logLik `-4.58264804445486806372400678810663521289825439453125`.
Independent logLik at that point: `-4.582648044454868803537150314` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.07e-08`.
Better feasible point: `[-0.02079845222106447774623738667, 0.9992068963337329412510712245, 1.100877862936304214093979601]`; logLik `-4.582648044454868383323971634`.
Stationary local point: `[-0.02079845222106447774623738667, 0.9992068963337329412510712245, 1.100877862936304214093979601]`; logLik `-4.582648044454868383323971634`. Verified coefficient relative error: `1.07e-08`.
Local information eigenvalue range: `[1.471402359715289097585510009, 1.471402359715289097585510009]`; per-observation stationarity residual `3.416814347555608244125794352e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6605: snorm, scale-1e-150

Data: `[4.4087305822829385e-150, -6.712233474398537e-151, -1.4004615090785175e-151, -4.485627924135602e-151, -4.856913427309694e-151, 1.1660316191367216e-151, 2.0436226031657505e-152, -1.1057748031821503e-150, -1.5018281477296033e-151, -7.455793360463969e-151, 3.6025543902349505e-151, 3.597089499548321e-151]`. Holds: `{'mean': -0.020798452221064478, 'sd': 0.9992068963337329}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6605` (run through the slot wrapper).
Returned: `{'mean': '-0.020798452221064478', 'sd': '0.99920689633373294', 'xi': '0.98413258918944213'}`, status `0`, logLik `-11.017174091748426434378416161052882671356201171875`.
Independent logLik at that point: `-11.01717409174843302147413508` (231 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.21e-08`.
Better feasible point: `[-0.02079845222106447774623738667, 0.9992068963337329412510712245, 0.984132577241800714201456551]`; logLik `-11.01717409174843126744978154`.
Stationary local point: `[-0.02079845222106447774623738667, 0.9992068963337329412510712245, 0.984132577241800714201456551]`; logLik `-11.01717409174843126744978154`. Verified coefficient relative error: `1.21e-08`.
Local information eigenvalue range: `[1.983477272977198391866400118, 1.983477272977198391866400118]`; per-observation stationarity residual `2.269087317040541307959269602e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6606: snorm, scale-1e+150

Data: `[4.4087305822829385e+150, -6.712233474398536e+149, -1.4004615090785175e+149, -4.4856279241356025e+149, -4.856913427309694e+149, 1.1660316191367217e+149, 2.0436226031657506e+148, -1.1057748031821504e+150, -1.5018281477296034e+149, -7.455793360463969e+149, 3.60255439023495e+149, 3.597089499548321e+149]`. Holds: `{'mean': -0.020798452221064478, 'sd': 0.9992068963337329}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6606` (run through the slot wrapper).
Returned: `{'mean': '-0.020798452221064478', 'sd': '0.99920689633373294', 'xi': '1.333978898647928'}`, status `0`, logLik `-8.9482866372466577833028502464806133035850584529012985506693461141858198422381959080169676947866151641301366561507062401942207691507111250562816235923449019481154847598191080258468267159245718982238451177066445753329346584186812855e+300`.
Independent logLik at that point: `-8.94828663724666322662656858e+300` (230 digits).
Evidence: interior dimensionless free score exceeds 1e-3 with feasible ascent. Worst relative discrepancy: `6.08e-16`.
Better feasible point: `[-0.02079845222106447774623738667, 0.9992068963337329412510712245, 1.33397889105100134251960536]`; logLik `-8.948286637246662386697571343e+300`.
Stationary local point: `[-0.02079845222106447774623738667, 0.9992068963337329412510712245, 1.33397889105100134251960536]`; logLik `-8.948286637246662386697571343e+300`. Verified coefficient relative error: `—`.
Scaled free-coordinate score (xi): `-2.458115877392143562137562783e+292`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6609: snorm, limit-concentration

Data: `[-0.14004587081554995, -0.14004601086170085, -0.14004615090785175, -0.14004629095400264, -0.14004643100015357]`. Holds: `{'mean': -0.020798452221064478, 'sd': 0.9992068963337329}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6609` (run through the slot wrapper).
Returned: `{'mean': '-0.020798452221064478', 'sd': '0.99920689633373294', 'xi': '1.1008778495807496'}`, status `0`, logLik `-4.582648044454945335246520699001848697662353515625`.
Independent logLik at that point: `-4.582648044454947164171262195` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.21e-08`.
Better feasible point: `[-0.02079845222106447774623738667, 0.9992068963337329412510712245, 1.100877862936351946493098783]`; logLik `-4.58264804445494662276887682`.
Stationary local point: `[-0.02079845222106447774623738667, 0.9992068963337329412510712245, 1.100877862936351946493098783]`; logLik `-4.58264804445494662276887682`. Verified coefficient relative error: `1.21e-08`.
Local information eigenvalue range: `[1.471402355466463774842630988, 1.471402355466463774842630988]`; per-observation stationarity residual `9.415065512080495472261579398e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6610: snorm, interior

Data: `[4.408730582282939, -0.6712233474398537, -0.14004615090785175, -0.44856279241356023, -0.48569134273096937, 0.11660316191367216, 0.020436226031657505, -1.1057748031821504, -0.15018281477296033, -0.7455793360463969, 0.36025543902349505, 0.3597089499548321]`. Holds: `{'xi': 3.394979982095818}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6610` (run through the slot wrapper).
Returned: `{'mean': '0.30945452988959476', 'sd': '1.1245620445368518', 'xi': '3.3949799820958182'}`, status `0`, logLik `-16.769031057738640555498932371847331523895263671875`.
Independent logLik at that point: `-16.76903105773864735686323889` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.2e-08`.
Better feasible point: `[0.3094545618947843099483484318, 1.124562069979281661127707681, 3.394979982095818193954528397]`; logLik `-16.76903105773864109599876637`.
Stationary local point: `[0.3094545618947843099483484318, 1.124562069979281661127707681, 3.394979982095818193954528397]`; logLik `-16.76903105773864109599876637`. Verified coefficient relative error: `3.2e-08`.
Local information eigenvalue range: `[1.891053946453242366951447527, 118.8381163246675356685582642]`; per-observation stationarity residual `7.65897342995655284407033219e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6617: snorm, constant--0.140046-1e-12

Data: `[-0.14004615090813183, -0.1400461509079918, -0.14004615090785175, -0.1400461509077117, -0.1400461509074316]`. Holds: `{'xi': 3.394979982095818}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6617` (run through the slot wrapper).
Returned: `{'mean': '-0.14004615090783884', 'sd': '2.4141431810781042e-13', 'xi': '3.3949799820958182'}`, status `0`, logLik `138.8611811239048847710364498198032379150390625`.
Independent logLik at that point: `138.8611810499543059546533924` (70 digits).
Evidence: interior dimensionless free score exceeds 1e-3 with feasible ascent. Worst relative discrepancy: `5.33e-10`.
Better feasible point: `[-0.1400461509078388273090722003, 2.414143143329545637434876193e-13, 3.394979982095818193954528397]`; logLik `138.8611811239048819927718911`.
Stationary local point: `[-0.1400461509078388273090722003, 2.414143143329545637434876193e-13, 3.394979982095818193954528397]`; logLik `138.8611811239048819927718911`. Verified coefficient relative error: `—`.
Scaled free-coordinate score (mean): `0.00162942852565243056470631076`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6620: snorm, constant-1-1e-12

Data: `[0.999999999998, 0.999999999999, 1.0, 1.000000000001, 1.000000000003]`. Holds: `{'xi': 3.394979982095818}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6620` (run through the slot wrapper).
Returned: `{'mean': '1.0000000000000924', 'sd': '1.7238563208214341e-12', 'xi': '3.3949799820958182'}`, status `0`, logLik `129.032158620939952697881381027400493621826171875`.
Independent logLik at that point: `129.0321585125760821543069001` (70 digits).
Evidence: interior dimensionless free score exceeds 1e-3 with feasible ascent. Worst relative discrepancy: `8.4e-10`.
Better feasible point: `[1.00000000000009226067309997, 1.723856316498903265262911811e-12, 3.394979982095818193954528397]`; logLik `129.0321586209399408720724277`.
Stationary local point: `[1.00000000000009226067309997, 1.723856316498903265262911811e-12, 3.394979982095818193954528397]`; logLik `129.0321586209399408720724277`. Verified coefficient relative error: `—`.
Scaled free-coordinate score (mean): `-0.001972448654115607250258220192`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6622: snorm, scale-1e+150

Data: `[4.4087305822829385e+150, -6.712233474398536e+149, -1.4004615090785175e+149, -4.4856279241356025e+149, -4.856913427309694e+149, 1.1660316191367217e+149, 2.0436226031657506e+148, -1.1057748031821504e+150, -1.5018281477296034e+149, -7.455793360463969e+149, 3.60255439023495e+149, 3.597089499548321e+149]`. Holds: `{'xi': 3.394979982095818}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6622` (run through the slot wrapper).
Returned: `{'mean': '3.0945455720645982e+149', 'sd': '1.1245620689905199e+150', 'xi': '3.3949799820958182'}`, status `0`, logLik `-4161.4221984470214010798372328281402587890625`.
Independent logLik at that point: `-4161.422198447020872369453551` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.52e-08`.
Better feasible point: `[3.094545618947843072460125288e+149, 1.124562069979281638153560789e+150, 3.394979982095818193954528397]`; logLik `-4161.422198447020872083230387`.
Stationary local point: `[3.094545618947843072460125288e+149, 1.124562069979281638153560789e+150, 3.394979982095818193954528397]`; logLik `-4161.422198447020872083230387`. Verified coefficient relative error: `1.52e-08`.
Local information eigenvalue range: `[1.891054024142984784201732104, 118.8381166107517938890811652]`; per-observation stationarity residual `2.071929121578569445190348219e-65`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6627: snorm, tiny-2

Data: `[4.408730582282939, -0.6712233474398537]`. Holds: `{'mean': -0.020798452221064478, 'xi': 3.394979982095818}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6627` (run through the slot wrapper).
Returned: `{'mean': '-0.020798452221064478', 'sd': '2.4525656420355535', 'xi': '3.3949799820958182'}`, status `0`, logLik `-5.23577095648479851064394097193144261837005615234375`.
Independent logLik at that point: `-5.235770956484799141281015082` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.77e-08`.
Better feasible point: `[-0.02079845222106447774623738667, 2.45256559872560036763687938, 3.394979982095818193954528397]`; logLik `-5.235770956484798626900162388`.
Stationary local point: `[-0.02079845222106447774623738667, 2.45256559872560036763687938, 3.394979982095818193954528397]`; logLik `-5.235770956484798626900162388`. Verified coefficient relative error: `1.77e-08`.
Local information eigenvalue range: `[1.649493690855124108313655892, 1.649493690855124108313655892]`; per-observation stationarity residual `7.024948197996064652558647239e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6629: snorm, tiny-5

Data: `[4.408730582282939, -0.6712233474398537, -0.14004615090785175, -0.44856279241356023, -0.48569134273096937]`. Holds: `{'mean': -0.020798452221064478, 'xi': 3.394979982095818}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6629` (run through the slot wrapper).
Returned: `{'mean': '-0.020798452221064478', 'sd': '1.3950553668975514', 'xi': '3.3949799820958182'}`, status `0`, logLik `-9.84336647667972641784217557869851589202880859375`.
Independent logLik at that point: `-9.84336647667972843742635776` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.9e-08`.
Better feasible point: `[-0.02079845222106447774623738667, 1.395055340441214014818889574, 3.394979982095818193954528397]`; logLik `-9.843366476679726801466492723`.
Stationary local point: `[-0.02079845222106447774623738667, 1.395055340441214014818889574, 3.394979982095818193954528397]`; logLik `-9.843366476679726801466492723`. Verified coefficient relative error: `1.9e-08`.
Local information eigenvalue range: `[1.819516917938065843847256712, 1.819516917938065843847256712]`; per-observation stationarity residual `1.614825866510454697500794554e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6634: snorm, constant-1-0.0001

Data: `[0.9998, 0.9999, 1.0, 1.0001, 1.0003]`. Holds: `{'mean': -0.020798452221064478, 'xi': 3.394979982095818}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6634` (run through the slot wrapper).
Returned: `{'mean': '-0.020798452221064478', 'sd': '0.91054547906428629', 'xi': '3.3949799820958182'}`, status `0`, logLik `-8.534031878174829444105853326618671417236328125`.
Independent logLik at that point: `-8.53403187817482958585567932` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.47e-08`.
Better feasible point: `[-0.02079845222106447774623738667, 0.910545465636651899700899222, 3.394979982095818193954528397]`; logLik `-8.534031878174828775808633831`.
Stationary local point: `[-0.02079845222106447774623738667, 0.910545465636651899700899222, 3.394979982095818193954528397]`; logLik `-8.534031878174828775808633831`. Verified coefficient relative error: `1.47e-08`.
Local information eigenvalue range: `[1.489959669645443362215499391, 1.489959669645443362215499391]`; per-observation stationarity residual `1.238044380235208577320078557e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6635: snorm, constant-1-1e-08

Data: `[0.99999998, 0.99999999, 1.0, 1.00000001, 1.00000003]`. Holds: `{'mean': -0.020798452221064478, 'xi': 3.394979982095818}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6635` (run through the slot wrapper).
Returned: `{'mean': '-0.020798452221064478', 'sd': '0.91052764356577953', 'xi': '3.3949799820958182'}`, status `0`, logLik `-8.5339338916065088369578006677329540252685546875`.
Independent logLik at that point: `-8.533933891606509276071793402` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.65e-08`.
Better feasible point: `[-0.02079845222106447774623738667, 0.9105276193974615034676859313, 3.394979982095818193954528397]`; logLik `-8.533933891606506651725774664`.
Stationary local point: `[-0.02079845222106447774623738667, 0.9105276193974615034676859313, 3.394979982095818193954528397]`; logLik `-8.533933891606506651725774664`. Verified coefficient relative error: `2.65e-08`.
Local information eigenvalue range: `[1.489959700033613760158450465, 1.489959700033613760158450465]`; per-observation stationarity residual `1.363891095737464946349600404e-58`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6636: snorm, constant-1-1e-12

Data: `[0.999999999998, 0.999999999999, 1.0, 1.000000000001, 1.000000000003]`. Holds: `{'mean': -0.020798452221064478, 'xi': 3.394979982095818}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6636` (run through the slot wrapper).
Returned: `{'mean': '-0.020798452221064478', 'sd': '0.91052763065892106', 'xi': '3.3949799820958182'}`, status `0`, logLik `-8.533933881811233135294969542883336544036865234375`.
Independent logLik at that point: `-8.533933881811233860672037976` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.43e-08`.
Better feasible point: `[-0.02079845222106447774623738667, 0.9105276176136880611301558196, 3.394979982095818193954528397]`; logLik `-8.533933881811233096076195021`.
Stationary local point: `[-0.02079845222106447774623738667, 0.9105276176136880611301558196, 3.394979982095818193954528397]`; logLik `-8.533933881811233096076195021`. Verified coefficient relative error: `1.43e-08`.
Local information eigenvalue range: `[1.489959663630658985355258214, 1.489959663630658985355258214]`; per-observation stationarity residual `9.827045625047056722750156166e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6637: snorm, scale-1e-150

Data: `[4.4087305822829385e-150, -6.712233474398537e-151, -1.4004615090785175e-151, -4.485627924135602e-151, -4.856913427309694e-151, 1.1660316191367216e-151, 2.0436226031657505e-152, -1.1057748031821503e-150, -1.5018281477296033e-151, -7.455793360463969e-151, 3.6025543902349505e-151, 3.597089499548321e-151]`. Holds: `{'mean': -0.020798452221064478, 'xi': 3.394979982095818}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6637` (run through the slot wrapper).
Returned: `{'mean': '-0.020798452221064478', 'sd': '0.018551718530955448', 'xi': '3.3949799820958182'}`, status `0`, logLik `26.24010058195799643954160274006426334381103515625`.
Independent logLik at that point: `26.24010058195799076174381481` (231 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.17e-08`.
Better feasible point: `[-0.02079845222106447774623738667, 0.01855171812779465400194375769, 3.394979982095818193954528397]`; logLik `26.24010058195799498370313285`.
Stationary local point: `[-0.02079845222106447774623738667, 0.01855171812779465400194375769, 3.394979982095818193954528397]`; logLik `26.24010058195799498370313285`. Verified coefficient relative error: `2.17e-08`.
Local information eigenvalue range: `[1.489959685695795266056744372, 1.489959685695795266056744372]`; per-observation stationarity residual `2.753635772986651351899059952e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6638: snorm, scale-1e+150

Data: `[4.4087305822829385e+150, -6.712233474398536e+149, -1.4004615090785175e+149, -4.4856279241356025e+149, -4.856913427309694e+149, 1.1660316191367217e+149, 2.0436226031657506e+148, -1.1057748031821504e+150, -1.5018281477296034e+149, -7.455793360463969e+149, 3.60255439023495e+149, 3.597089499548321e+149]`. Holds: `{'mean': -0.020798452221064478, 'xi': 3.394979982095818}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6638` (run through the slot wrapper).
Returned: `{'mean': '-0.020798452221064478', 'sd': '9.3185478766152322e+149', 'xi': '3.3949799820958182'}`, status `0`, logLik `-4162.1355689028014239738695323467254638671875`.
Independent logLik at that point: `-4162.135568902800965721639621` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.11e-08`.
Better feasible point: `[-0.02079845222106447774623738667, 9.318547979935703084420135382e+149, 3.394979982095818193954528397]`; logLik `-4162.135568902800959916873679`.
Stationary local point: `[-0.02079845222106447774623738667, 9.318547979935703084420135382e+149, 3.394979982095818193954528397]`; logLik `-4162.135568902800959916873679`. Verified coefficient relative error: `1.11e-08`.
Local information eigenvalue range: `[7.869681959730323912911032151, 7.869681959730323912911032151]`; per-observation stationarity residual `2.903605290856290454732822813e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6639: snorm, offset-1e8

Data: `[100000004.40873058, 99999999.32877666, 99999999.85995385, 99999999.55143721, 99999999.51430866, 100000000.11660317, 100000000.02043623, 99999998.8942252, 99999999.84981719, 99999999.25442067, 100000000.36025544, 100000000.35970895]`. Holds: `{'mean': -0.020798452221064478, 'xi': 3.394979982095818}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6639` (run through the slot wrapper).
Returned: `{'mean': '-0.020798452221064478', 'sd': '89197592.397137344', 'xi': '3.3949799820958182'}`, status `0`, logLik `-241.28258885306212278010207228362560272216796875`.
Independent logLik at that point: `-241.2825888530621282905173279` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.6e-08`.
Better feasible point: `[-0.02079845222106447774623738667, 89197590.08000825636175418576, 3.394979982095818193954528397]`; logLik `-241.2825888530621222577031545`.
Stationary local point: `[-0.02079845222106447774623738667, 89197590.08000825636175418576, 3.394979982095818193954528397]`; logLik `-241.2825888530621222577031545`. Verified coefficient relative error: `2.6e-08`.
Local information eigenvalue range: `[1.489959698347825335988929942, 1.489959698347825335988929942]`; per-observation stationarity residual `1.147968926574230213003674818e-58`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6646: snorm, ties

Data: `[4.408730582282939, 4.408730582282939, 4.408730582282939, 4.408730582282939, -0.6712233474398537, -0.14004615090785175]`. Holds: `{'sd': 0.9992068963337329, 'xi': 3.394979982095818}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6646` (run through the slot wrapper).
Returned: `{'mean': '0.64616842167985555', 'sd': '0.99920689633373294', 'xi': '3.3949799820958182'}`, status `0`, logLik `-24.266152484354552854028952424414455890655517578125`.
Independent logLik at that point: `-24.26615248435455858965604049` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.02e-08`.
Better feasible point: `[0.6461684114747979542901528946, 0.9992068963337329412510712245, 3.394979982095818193954528397]`; logLik `-24.2661524843545557871203428`.
Stationary local point: `[0.6461684114747979542901528946, 0.9992068963337329412510712245, 3.394979982095818193954528397]`; logLik `-24.2661524843545557871203428`. Verified coefficient relative error: `1.02e-08`.
Local information eigenvalue range: `[231.4826924877206831956289028, 231.4826924877206831956289028]`; per-observation stationarity residual `2.568850379320898089920311548e-54`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6656: snorm, limit-outlier

Data: `[4.408730582282939, -0.6712233474398537, -0.14004615090785175, -0.44856279241356023, 1000004.4087305823]`. Holds: `{'sd': 0.9992068963337329, 'xi': 3.394979982095818}`.
Call: `route_snorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6656` (run through the slot wrapper).
Returned: `{'mean': '1880.3025948252762', 'sd': '0.99920689633373294', 'xi': '3.3949799820958182'}`, status `0`, logLik `-194855341404.38275146484375`.
Independent logLik at that point: `-194855341404.3828271298161807` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.55e-07`.
Better feasible point: `[1880.301927058411655236884, 0.9992068963337329412510712245, 3.394979982095818193954528397]`; logLik `-194855341404.382780785038564`.
Stationary local point: `[1880.301927058411655236884, 0.9992068963337329412510712245, 3.394979982095818193954528397]`; logLik `-194855341404.382780785038564`. Verified coefficient relative error: `3.55e-07`.
Local information eigenvalue range: `[41573393529902.37216072755975, 41573393529902.37216072755975]`; per-observation stationarity residual `2.238531109185432914826187376e-44`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/snorm/fs.hpp:23` (special difference): `return 0.5*(std::lgamma(1.0/nu)-std::lgamma(3.0/nu));`. Execution-path causation remains open.

## Case 6711: sstd, constant--0.106887-0.0001

Data: `[-0.10690873946095238, -0.10689805072475353, -0.10688736198855467, -0.10687667325235581, -0.1068552957799581]`. Holds: `{'sd': 1.0184483076867987}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6711` (run through the slot wrapper).
Returned: `{'mean': '-0.10686250195917418', 'sd': '1.0184483076867987', 'nu': '2.0000000020612068', 'xi': '200.35531008984998'}`, status `0`, logLik `47.6780693332925267213795450516045093536376953125`.
Independent logLik at that point: `47.67806933329222735616328284` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `3.59e-08`.
Better feasible point: `[-0.1068625019591741831526121587, 1.018448307686798726123811321, 2.000000002061206760828326878, 202.3588631907484790417584009]`; logLik `47.6780710451169579880605905`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6721: sstd, limit-concentration

Data: `[-0.10688714821383069, -0.10688725510119268, -0.10688736198855467, -0.10688746887591664, -0.10688757576327865]`. Holds: `{'sd': 1.0184483076867987}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6721` (run through the slot wrapper).
Returned: `{'mean': '-0.1068396824291642', 'sd': '1.0184483076867987', 'nu': '2.0000000022147812', 'xi': '29.715590061450694'}`, status `0`, logLik `49.720154102884634994552470743656158447265625`.
Independent logLik at that point: `49.72015410288464090160363967` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `4.9e-06`.
Better feasible point: `[-0.1068396725342803152504353152, 1.018448307686798726123811321, 2.000000002214781247289465682, 30.12824535004660881501372906]`; logLik `49.72039794864781655984503369`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6734: sstd, scale-1e+150

Data: `[-6.644244801433567e+149, -2.523956013072597e+149, 3.361765600321436e+150, -8.303898773106027e+149, -6.540074278309951e+149, -8.524330983809122e+149, 9.672772989273258e+149, -1.0688736198855466e+149, 2.2048384943209636e+150, 1.5939491560138295e+150, -5.181460629130914e+149, 4.781099340707672e+149]`. Holds: `{'mean': 0.012734271748561354, 'sd': 1.0184483076867987}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6734` (run through the slot wrapper).
Returned: `{'mean': '0.012734271748561354', 'sd': '1.0184483076867987', 'nu': '2.0028805152440543', 'xi': '0.94534556504587375'}`, status `0`, logLik `-12512.632569799714474356733262538909912109375`.
Independent logLik at that point: `-12512.63256979971544603678431` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.12e-07`.
Better feasible point: `[0.01273427174856135365987874764, 1.018448307686798726123811321, 2.002880516736549181550688465, 0.9453456712233153681826597663]`; logLik `-12512.63256979971317538607939`.
Stationary local point: `[0.01273427174856135365987874764, 1.018448307686798726123811321, 2.002880516736549181550688465, 0.9453456712233153681826597663]`; logLik `-12512.63256979971317538607939`. Verified coefficient relative error: `1.12e-07`.
Local information eigenvalue range: `[8.738623316242372740228819015, 482774.1938470192098364923962]`; per-observation stationarity residual `3.628015873169252577738104487e-48`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6786: sstd, interior

Data: `[-0.6644244801433566, -0.2523956013072597, 3.3617656003214367, -0.8303898773106027, -0.6540074278309951, -0.8524330983809123, 0.9672772989273257, -0.10688736198855467, 2.204838494320964, 1.5939491560138295, -0.5181460629130914, 0.47810993407076724]`. Holds: `{'mean': 0.012734271748561354, 'sd': 1.0184483076867987, 'nu': 2.9622019441967624}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6786` (run through the slot wrapper).
Returned: `{'mean': '0.012734271748561354', 'sd': '1.0184483076867987', 'nu': '2.9622019441967624', 'xi': '2.7960656790290708'}`, status `0`, logLik `-18.42611607521177319313210318796336650848388671875`.
Independent logLik at that point: `-18.42611607521177908684481116` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.2e-08`.
Better feasible point: `[0.01273427174856135365987874764, 1.018448307686798726123811321, 2.96220194419676241537331407, 2.796065645411640879373489813]`; logLik `-18.42611607521177879571412368`.
Stationary local point: `[0.01273427174856135365987874764, 1.018448307686798726123811321, 2.96220194419676241537331407, 2.796065645411640879373489813]`; logLik `-18.42611607521177879571412368`. Verified coefficient relative error: `1.2e-08`.
Local information eigenvalue range: `[0.3356622190019352739512957187, 0.3356622190019352739512957187]`; per-observation stationarity residual `4.351243039403939284195789008e-64`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6789: sstd, tiny-5

Data: `[-0.6644244801433566, -0.2523956013072597, 3.3617656003214367, -0.8303898773106027, -0.6540074278309951]`. Holds: `{'mean': 0.012734271748561354, 'sd': 1.0184483076867987, 'nu': 2.9622019441967624}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6789` (run through the slot wrapper).
Returned: `{'mean': '0.012734271748561354', 'sd': '1.0184483076867987', 'nu': '2.9622019441967624', 'xi': '3.7060492823364313'}`, status `0`, logLik `-6.3501361592650145126981442444957792758941650390625`.
Independent logLik at that point: `-6.350136159265019761935040472` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.75e-08`.
Better feasible point: `[0.01273427174856135365987874764, 1.018448307686798726123811321, 2.96220194419676241537331407, 3.706049180284878932855109859]`; logLik `-6.35013615926501899551449467`.
Stationary local point: `[0.01273427174856135365987874764, 1.018448307686798726123811321, 2.96220194419676241537331407, 3.706049180284878932855109859]`; logLik `-6.35013615926501899551449467`. Verified coefficient relative error: `2.75e-08`.
Local information eigenvalue range: `[0.4043060965980080835875120509, 0.4043060965980080835875120509]`; per-observation stationarity residual `9.220566393813240980465908341e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6797: sstd, scale-1e-150

Data: `[-6.644244801433567e-151, -2.523956013072597e-151, 3.361765600321437e-150, -8.303898773106027e-151, -6.54007427830995e-151, -8.524330983809123e-151, 9.672772989273258e-151, -1.0688736198855467e-151, 2.2048384943209637e-150, 1.5939491560138296e-150, -5.181460629130914e-151, 4.781099340707672e-151]`. Holds: `{'mean': 0.012734271748561354, 'sd': 1.0184483076867987, 'nu': 2.9622019441967624}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6797` (run through the slot wrapper).
Returned: `{'mean': '0.012734271748561354', 'sd': '1.0184483076867987', 'nu': '2.9622019441967624', 'xi': '1.0127587756899732'}`, status `0`, logLik `-5.4943481725088592071415405371226370334625244140625`.
Independent logLik at that point: `-5.494348172508866340616127443` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.07e-08`.
Better feasible point: `[0.01273427174856135365987874764, 1.018448307686798726123811321, 2.96220194419676241537331407, 1.012758764885075422471757178]`; logLik `-5.494348172508862970110931487`.
Stationary local point: `[0.01273427174856135365987874764, 1.018448307686798726123811321, 2.96220194419676241537331407, 1.012758764885075422471757178]`; logLik `-5.494348172508862970110931487`. Verified coefficient relative error: `1.07e-08`.
Local information eigenvalue range: `[4.935310411412691053167521037, 4.935310411412691053167521037]`; per-observation stationarity residual `1.082180578218370157563424563e-58`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6798: sstd, scale-1e+150

Data: `[-6.644244801433567e+149, -2.523956013072597e+149, 3.361765600321436e+150, -8.303898773106027e+149, -6.540074278309951e+149, -8.524330983809122e+149, 9.672772989273258e+149, -1.0688736198855466e+149, 2.2048384943209636e+150, 1.5939491560138295e+150, -5.181460629130914e+149, 4.781099340707672e+149]`. Holds: `{'mean': 0.012734271748561354, 'sd': 1.0184483076867987, 'nu': 2.9622019441967624}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6798` (run through the slot wrapper).
Returned: `{'mean': '0.012734271748561354', 'sd': '1.0184483076867987', 'nu': '2.9622019441967624', 'xi': '0.92108989490727566'}`, status `0`, logLik `-16412.3085908076245686970651149749755859375`.
Independent logLik at that point: `-16412.30859080762733809831265` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.54e-08`.
Better feasible point: `[0.01273427174856135365987874764, 1.018448307686798726123811321, 2.96220194419676241537331407, 0.9210898346735174635658939457]`; logLik `-16412.3085908076271362489849`.
Stationary local point: `[0.01273427174856135365987874764, 1.018448307686798726123811321, 2.96220194419676241537331407, 0.9210898346735174635658939457]`; logLik `-16412.3085908076271362489849`. Verified coefficient relative error: `6.54e-08`.
Local information eigenvalue range: `[7.866837716757204703424712636, 7.866837716757204703424712636]`; per-observation stationarity residual `5.790586159182555603966125068e-57`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6800: sstd, limit-outlier

Data: `[-0.6644244801433566, -0.2523956013072597, 3.3617656003214367, -0.8303898773106027, 1000003.3617656003]`. Holds: `{'mean': 0.012734271748561354, 'sd': 1.0184483076867987, 'nu': 2.9622019441967624}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6800` (run through the slot wrapper).
Returned: `{'mean': '0.012734271748561354', 'sd': '1.0184483076867987', 'nu': '2.9622019441967624', 'xi': '3.6570485096565695'}`, status `0`, logLik `-60.03180973927414498803045717068016529083251953125`.
Independent logLik at that point: `-60.03180973927415332540286036` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.57e-08`.
Better feasible point: `[0.01273427174856135365987874764, 1.018448307686798726123811321, 2.96220194419676241537331407, 3.657048452392001152824621271]`; logLik `-60.03180973927415307768918357`.
Stationary local point: `[0.01273427174856135365987874764, 1.018448307686798726123811321, 2.96220194419676241537331407, 3.657048452392001152824621271]`; logLik `-60.03180973927415307768918357`. Verified coefficient relative error: `1.57e-08`.
Local information eigenvalue range: `[0.4041098564411868332696890251, 0.4041098564411868332696890251]`; per-observation stationarity residual `1.597984735545271801473888923e-62`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6802: sstd, interior

Data: `[-0.6644244801433566, -0.2523956013072597, 3.3617656003214367, -0.8303898773106027, -0.6540074278309951, -0.8524330983809123, 0.9672772989273257, -0.10688736198855467, 2.204838494320964, 1.5939491560138295, -0.5181460629130914, 0.47810993407076724]`. Holds: `{'xi': 1.9114750836827714}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6802` (run through the slot wrapper).
Returned: `{'mean': '0.29518058190671503', 'sd': '1.6071854993447479', 'nu': '2.8634042832983746', 'xi': '1.9114750836827714'}`, status `0`, logLik `-17.64652781108200230164584354497492313385009765625`.
Independent logLik at that point: `-17.64652781108199745040936026` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.74e-07`.
Better feasible point: `[0.2951806710061329338903535887, 1.607184938769134764479795785, 2.8634053537178894241184294, 1.911475083682771414217427264]`; logLik `-17.64652781108188666503561557`.
Stationary local point: `[0.2951806710061329338903535887, 1.607184938769134764479795785, 2.8634053537178894241184294, 1.911475083682771414217427264]`; logLik `-17.64652781108188666503561557`. Verified coefficient relative error: `3.74e-07`.
Local information eigenvalue range: `[0.01119256244045796505542478362, 80.00192586627810518046131348]`; per-observation stationarity residual `9.988581086308477765846614364e-70`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6813: sstd, scale-1e-150

Data: `[-6.644244801433567e-151, -2.523956013072597e-151, 3.361765600321437e-150, -8.303898773106027e-151, -6.54007427830995e-151, -8.524330983809123e-151, 9.672772989273258e-151, -1.0688736198855467e-151, 2.2048384943209637e-150, 1.5939491560138296e-150, -5.181460629130914e-151, 4.781099340707672e-151]`. Holds: `{'xi': 1.9114750836827714}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6813` (run through the slot wrapper).
Returned: `{'mean': '2.9518057962763219e-151', 'sd': '1.6071849464839404e-150', 'nu': '2.8634052538530135', 'xi': '1.9114750836827714'}`, status `0`, logLik `4127.00663957820142968557775020599365234375`.
Independent logLik at that point: `4127.006639578200250357448348` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.49e-08`.
Better feasible point: `[2.951806710061329568610623829e-151, 1.607184938769134583614146045e-150, 2.863405353717889813748297615, 1.911475083682771414217427264]`; logLik `4127.006639578200344274126503`.
Stationary local point: `[2.951806710061329568610623829e-151, 1.607184938769134583614146045e-150, 2.863405353717889813748297615, 1.911475083682771414217427264]`; logLik `4127.006639578200344274126503`. Verified coefficient relative error: `3.49e-08`.
Local information eigenvalue range: `[0.01119256199810209725867171428, 80.0019259085382101325515882]`; per-observation stationarity residual `1.485951504907110596379860516e-45`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6814: sstd, scale-1e+150

Data: `[-6.644244801433567e+149, -2.523956013072597e+149, 3.361765600321436e+150, -8.303898773106027e+149, -6.540074278309951e+149, -8.524330983809122e+149, 9.672772989273258e+149, -1.0688736198855466e+149, 2.2048384943209636e+150, 1.5939491560138295e+150, -5.181460629130914e+149, 4.781099340707672e+149]`. Holds: `{'xi': 1.9114750836827714}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6814` (run through the slot wrapper).
Returned: `{'mean': '2.9518058937301898e+149', 'sd': '1.6071849396851037e+150', 'nu': '2.8634053117849989', 'xi': '1.9114750836827714'}`, status `0`, logLik `-4162.299695200365022174082696437835693359375`.
Independent logLik at that point: `-4162.299695200364227677431826` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.77e-07`.
Better feasible point: `[2.951806710061328854933197626e+149, 1.607184938769134222979241335e+150, 2.863405353717890244323251585, 1.911475083682771414217427264]`; logLik `-4162.299695200364117314349811`.
Stationary local point: `[2.951806710061328854933197626e+149, 1.607184938769134222979241335e+150, 2.863405353717890244323251585, 1.911475083682771414217427264]`; logLik `-4162.299695200364117314349811`. Verified coefficient relative error: `2.77e-07`.
Local information eigenvalue range: `[0.01119256216337022612541861195, 80.00192605032941573590947981]`; per-observation stationarity residual `6.1541371979141084656105188e-47`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6815: sstd, offset-1e8

Data: `[99999999.33557552, 99999999.7476044, 100000003.3617656, 99999999.16961013, 99999999.34599257, 99999999.1475669, 100000000.9672773, 99999999.89311264, 100000002.2048385, 100000001.59394915, 99999999.48185393, 100000000.47810994]`. Holds: `{'xi': 1.9114750836827714}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6815` (run through the slot wrapper).
Returned: `{'mean': '100000000.29518059', 'sd': '1.6071867095360788', 'nu': '2.8634021834022625', 'xi': '1.9114750836827714'}`, status `0`, logLik `-17.646527816002755884028374566696584224700927734375`.
Independent logLik at that point: `-17.64652781600274862502622798` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.11e-06`.
Better feasible point: `[100000000.2951806705084850508, 1.607184933384907616869614103, 2.863405364414072122135886789, 1.911475083682771414217427264]`; logLik `-17.64652781600247542727244143`.
Stationary local point: `[100000000.2951806705084850508, 1.607184933384907616869614103, 2.863405364414072122135886789, 1.911475083682771414217427264]`; logLik `-17.64652781600247542727244143`. Verified coefficient relative error: `1.11e-06`.
Local information eigenvalue range: `[0.01119256319255587176045936151, 80.00192554118154875874067578]`; per-observation stationarity residual `3.32131025955811132061466636e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6818: sstd, interior

Data: `[-0.6644244801433566, -0.2523956013072597, 3.3617656003214367, -0.8303898773106027, -0.6540074278309951, -0.8524330983809123, 0.9672772989273257, -0.10688736198855467, 2.204838494320964, 1.5939491560138295, -0.5181460629130914, 0.47810993407076724]`. Holds: `{'mean': 0.012734271748561354, 'xi': 1.9114750836827714}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6818` (run through the slot wrapper).
Returned: `{'mean': '0.012734271748561354', 'sd': '1.6312624708166024', 'nu': '2.4225427413446297', 'xi': '1.9114750836827714'}`, status `0`, logLik `-18.019556972797492022664300748147070407867431640625`.
Independent logLik at that point: `-18.01955697279749242143928715` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.29e-06`.
Better feasible point: `[0.01273427174856135365987874764, 1.631266203321342484434956253, 2.422540270459610955380231942, 1.911475083682771414217427264]`; logLik `-18.01955697279660761556711057`.
Stationary local point: `[0.01273427174856135365987874764, 1.631266203321342484434956253, 2.422540270459610955380231942, 1.911475083682771414217427264]`; logLik `-18.01955697279660761556711057`. Verified coefficient relative error: `2.29e-06`.
Local information eigenvalue range: `[0.02311855800254642636501967345, 20.03377085816186474891451474]`; per-observation stationarity residual `1.975452876347887109512027322e-69`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6830: sstd, scale-1e+150

Data: `[-6.644244801433567e+149, -2.523956013072597e+149, 3.361765600321436e+150, -8.303898773106027e+149, -6.540074278309951e+149, -8.524330983809122e+149, 9.672772989273258e+149, -1.0688736198855466e+149, 2.2048384943209636e+150, 1.5939491560138295e+150, -5.181460629130914e+149, 4.781099340707672e+149]`. Holds: `{'mean': 0.012734271748561354, 'xi': 1.9114750836827714}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6830` (run through the slot wrapper).
Returned: `{'mean': '0.012734271748561354', 'sd': '1.6423602176606602e+150', 'nu': '2.4025213772734451', 'xi': '1.9114750836827714'}`, status `0`, logLik `-4162.708886554746641195379197597503662109375`.
Independent logLik at that point: `-4162.708886554746259366503996` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4e-06`.
Better feasible point: `[0.01273427174856135365987874764, 1.642353648887578132071373598e+150, 2.402525147166692669998754268, 1.911475083682771414217427264]`; logLik `-4162.708886554741381105790857`.
Stationary local point: `[0.01273427174856135365987874764, 1.642353648887578132071373598e+150, 2.402525147166692669998754268, 1.911475083682771414217427264]`; logLik `-4162.708886554741381105790857`. Verified coefficient relative error: `4e-06`.
Local information eigenvalue range: `[0.02264978798452896682332124095, 21.98200536911047461002100291]`; per-observation stationarity residual `6.532890075629729233004449405e-64`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6834: sstd, interior

Data: `[-0.6644244801433566, -0.2523956013072597, 3.3617656003214367, -0.8303898773106027, -0.6540074278309951, -0.8524330983809123, 0.9672772989273257, -0.10688736198855467, 2.204838494320964, 1.5939491560138295, -0.5181460629130914, 0.47810993407076724]`. Holds: `{'sd': 1.0184483076867987, 'xi': 1.9114750836827714}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6834` (run through the slot wrapper).
Returned: `{'mean': '0.22756628668549417', 'sd': '1.0184483076867987', 'nu': '8.5667622416786564', 'xi': '1.9114750836827714'}`, status `0`, logLik `-17.887421177924853310514663462527096271514892578125`.
Independent logLik at that point: `-17.88742117792485185636349427` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.66e-07`.
Better feasible point: `[0.227566326809078995922819367, 1.018448307686798726123811321, 8.566767091949195958385008867, 1.911475083682771414217427264]`; logLik `-17.88742117792480874726069925`.
Stationary local point: `[0.227566326809078995922819367, 1.018448307686798726123811321, 8.566767091949195958385008867, 1.911475083682771414217427264]`; logLik `-17.88742117792480874726069925`. Verified coefficient relative error: `5.66e-07`.
Local information eigenvalue range: `[0.01658690813614675616002635587, 54.22541429375831886921128394]`; per-observation stationarity residual `2.837259887261780807950068975e-46`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6835: sstd, tiny-2

Data: `[-0.6644244801433566, -0.2523956013072597]`. Holds: `{'sd': 1.0184483076867987, 'xi': 1.9114750836827714}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6835` (run through the slot wrapper).
Returned: `{'mean': '-0.44023503107248119', 'sd': '1.0184483076867987', 'nu': '2.0697749154455947', 'xi': '1.9114750836827714'}`, status `0`, logLik `0.36573769517914922477785921728354878723621368408203125`.
Independent logLik at that point: `0.3657376951791490948439757178` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.18e-08`.
Better feasible point: `[-0.4402350193187707622068150963, 1.018448307686798726123811321, 2.069774920898383587248030801, 1.911475083682771414217427264]`; logLik `0.3657376951791523087906498323`.
Stationary local point: `[-0.4402350193187707622068150963, 1.018448307686798726123811321, 2.069774920898383587248030801, 1.911475083682771414217427264]`; logLik `0.3657376951791523087906498323`. Verified coefficient relative error: `1.18e-08`.
Local information eigenvalue range: `[2.25997693422745387735623929, 2306.834290149687758175385974]`; per-observation stationarity residual `2.932981302465803518781820713e-55`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6836: sstd, tiny-3

Data: `[-0.6644244801433566, -0.2523956013072597, 3.3617656003214367]`. Holds: `{'sd': 1.0184483076867987, 'xi': 1.9114750836827714}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6836` (run through the slot wrapper).
Returned: `{'mean': '-0.067736857795623884', 'sd': '1.0184483076867987', 'nu': '2.6857468034669996', 'xi': '1.9114750836827714'}`, status `0`, logLik `-5.6160427824578977862302053836174309253692626953125`.
Independent logLik at that point: `-5.616042782457898266593418012` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.42e-07`.
Better feasible point: `[-0.06773700615841252930217450369, 1.018448307686798726123811321, 2.685747454277372693991983392, 1.911475083682771414217427264]`; logLik `-5.616042782455439888830357865`.
Stationary local point: `[-0.06773700615841252930217450369, 1.018448307686798726123811321, 2.685747454277372693991983392, 1.911475083682771414217427264]`; logLik `-5.616042782455439888830357865`. Verified coefficient relative error: `2.42e-07`.
Local information eigenvalue range: `[1.102553929715089547177608317, 235.0369403060413347683424429]`; per-observation stationarity residual `1.289226657649564809302478913e-41`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6837: sstd, tiny-5

Data: `[-0.6644244801433566, -0.2523956013072597, 3.3617656003214367, -0.8303898773106027, -0.6540074278309951]`. Holds: `{'sd': 1.0184483076867987, 'xi': 1.9114750836827714}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6837` (run through the slot wrapper).
Returned: `{'mean': '-0.44603690123210726', 'sd': '1.0184483076867987', 'nu': '2.1909950298337826', 'xi': '1.9114750836827714'}`, status `0`, logLik `-5.78996777982315879995667273760773241519927978515625`.
Independent logLik at that point: `-5.789967779823156860413884498` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.13e-08`.
Better feasible point: `[-0.4460369425273095925710892157, 1.018448307686798726123811321, 2.190994971837109000956800018, 1.911475083682771414217427264]`; logLik `-5.789967779823120090138637455`.
Stationary local point: `[-0.4460369425273095925710892157, 1.018448307686798726123811321, 2.190994971837109000956800018, 1.911475083682771414217427264]`; logLik `-5.789967779823120090138637455`. Verified coefficient relative error: `4.13e-08`.
Local information eigenvalue range: `[15.49629114594763429322834807, 562.0955791770715118474634824]`; per-observation stationarity residual `1.015544969300704068306743342e-49`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6846: sstd, scale-1e+150

Data: `[-6.644244801433567e+149, -2.523956013072597e+149, 3.361765600321436e+150, -8.303898773106027e+149, -6.540074278309951e+149, -8.524330983809122e+149, 9.672772989273258e+149, -1.0688736198855466e+149, 2.2048384943209636e+150, 1.5939491560138295e+150, -5.181460629130914e+149, 4.781099340707672e+149]`. Holds: `{'sd': 1.0184483076867987, 'xi': 1.9114750836827714}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6846` (run through the slot wrapper).
Returned: `{'mean': '1.2546662509702619e+150', 'sd': '1.0184483076867987', 'nu': '2.0028745906195726', 'xi': '1.9114750836827714'}`, status `0`, logLik `-12557.538511191329234861768782138824462890625`.
Independent logLik at that point: `-12557.53851119133224807552221` (230 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.0877`.
Better feasible point: `[-6.644244801433566551942132554e+149, 1.018448307686798726123811321, 2.00287459061957262562714277, 1.911475083682771414217427264]`; logLik `-11456.18543800498151240179656`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6847: sstd, offset-1e8

Data: `[99999999.33557552, 99999999.7476044, 100000003.3617656, 99999999.16961013, 99999999.34599257, 99999999.1475669, 100000000.9672773, 99999999.89311264, 100000002.2048385, 100000001.59394915, 99999999.48185393, 100000000.47810994]`. Holds: `{'sd': 1.0184483076867987, 'xi': 1.9114750836827714}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6847` (run through the slot wrapper).
Returned: `{'mean': '100000000.22756627', 'sd': '1.0184483076867987', 'nu': '8.5667608582091184', 'xi': '1.9114750836827714'}`, status `0`, logLik `-17.887421180992109981389148742891848087310791015625`.
Independent logLik at that point: `-17.88742118099211650019806759` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `7.46e-07`.
Better feasible point: `[100000000.2275663283519068312, 1.018448307686798726123811321, 8.566767250161764063952134382, 1.911475083682771414217427264]`; logLik `-17.8874211809920455321551196`.
Stationary local point: `[100000000.2275663283519068312, 1.018448307686798726123811321, 8.566767250161764063952134382, 1.911475083682771414217427264]`; logLik `-17.8874211809920455321551196`. Verified coefficient relative error: `7.46e-07`.
Local information eigenvalue range: `[0.01658690127618802199481328766, 54.22541383001165721438542756]`; per-observation stationarity residual `2.213258721975047757617409311e-45`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6848: sstd, limit-outlier

Data: `[-0.6644244801433566, -0.2523956013072597, 3.3617656003214367, -0.8303898773106027, 1000003.3617656003]`. Holds: `{'sd': 1.0184483076867987, 'xi': 1.9114750836827714}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6848` (run through the slot wrapper).
Returned: `{'mean': '500001.25290008768', 'sd': '1.0184483076867987', 'nu': '2.0717238665270092', 'xi': '1.9114750836827714'}`, status `0`, logLik `-230.646607131776789856303366832435131072998046875`.
Independent logLik at that point: `-230.6466071317768439821203132` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.773`.
Better feasible point: `[-0.6644244801433566438575439861, 1.018448307686798726123811321, 2.071723866527009239035805876, 1.911475083682771414217427264]`; logLik `-52.45081072548743970000698783`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6850: sstd, interior

Data: `[-0.6644244801433566, -0.2523956013072597, 3.3617656003214367, -0.8303898773106027, -0.6540074278309951, -0.8524330983809123, 0.9672772989273257, -0.10688736198855467, 2.204838494320964, 1.5939491560138295, -0.5181460629130914, 0.47810993407076724]`. Holds: `{'mean': 0.012734271748561354, 'sd': 1.0184483076867987, 'xi': 1.9114750836827714}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6850` (run through the slot wrapper).
Returned: `{'mean': '0.012734271748561354', 'sd': '1.0184483076867987', 'nu': '3.9294881302767553', 'xi': '1.9114750836827714'}`, status `0`, logLik `-18.174914624640994276205674395896494388580322265625`.
Independent logLik at that point: `-18.1749146246410008347446289` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.98e-08`.
Better feasible point: `[0.01273427174856135365987874764, 1.018448307686798726123811321, 3.929487934508635017590175994, 1.911475083682771414217427264]`; logLik `-18.1749146246409935747243601`.
Stationary local point: `[0.01273427174856135365987874764, 1.018448307686798726123811321, 3.929487934508635017590175994, 1.911475083682771414217427264]`; logLik `-18.1749146246409935747243601`. Verified coefficient relative error: `4.98e-08`.
Local information eigenvalue range: `[0.4875000684277569068726444626, 0.4875000684277569068726444626]`; per-observation stationarity residual `2.328312252324646485454230113e-54`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6878: sstd, scale-1e+150

Data: `[-6.644244801433567e+149, -2.523956013072597e+149, 3.361765600321436e+150, -8.303898773106027e+149, -6.540074278309951e+149, -8.524330983809122e+149, 9.672772989273258e+149, -1.0688736198855466e+149, 2.2048384943209636e+150, 1.5939491560138295e+150, -5.181460629130914e+149, 4.781099340707672e+149]`. Holds: `{'nu': 2.9622019441967624, 'xi': 1.9114750836827714}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6878` (run through the slot wrapper).
Returned: `{'mean': '2.9459198434913515e+149', 'sd': '1.5550589285691862e+150', 'nu': '2.9622019441967624', 'xi': '1.9114750836827714'}`, status `0`, logLik `-4162.2998519000857413630001246929168701171875`.
Independent logLik at that point: `-4162.299851900085345323678941` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.27e-08`.
Better feasible point: `[2.945919776638583582756572443e+149, 1.55505892590149370911178042e+150, 2.96220194419676241537331407, 1.911475083682771414217427264]`; logLik `-4162.299851900085344729031373`.
Stationary local point: `[2.945919776638583582756572443e+149, 1.55505892590149370911178042e+150, 2.96220194419676241537331407, 1.911475083682771414217427264]`; logLik `-4162.299851900085344729031373`. Verified coefficient relative error: `2.27e-08`.
Local information eigenvalue range: `[0.8344038575555232183968758845, 73.72490065297404725893994848]`; per-observation stationarity residual `4.883773593054614549205802651e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6880: sstd, limit-outlier

Data: `[-0.6644244801433566, -0.2523956013072597, 3.3617656003214367, -0.8303898773106027, 1000003.3617656003]`. Holds: `{'nu': 2.9622019441967624, 'xi': 1.9114750836827714}`.
Call: `route_sstd(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6880` (run through the slot wrapper).
Returned: `{'mean': '2.6364030898548663', 'sd': '5.4534179660154649', 'nu': '2.9622019441967624', 'xi': '1.9114750836827714'}`, status `0`, logLik `-57.3834590877636543382322997786104679107666015625`.
Independent logLik at that point: `-57.38345908776139412415306934` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.71e-07`.
Better feasible point: `[2.636404332158574659717140729, 5.453418363819011003784179425, 2.96220194419676241537331407, 1.911475083682771414217427264]`; logLik `-57.38345908776026016705481754`.
Stationary local point: `[2.636404332158574659717140729, 5.453418363819011003784179425, 2.96220194419676241537331407, 1.911475083682771414217427264]`; logLik `-57.38345908776026016705481754`. Verified coefficient relative error: `4.71e-07`.
Local information eigenvalue range: `[0.3173470961694408062852625731, 450977635170.4548450335078189]`; per-observation stationarity residual `1.49946738860215557634201706e-42`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/sstd/hold_search.hpp:19` (special difference): `return std::exp(0.5*std::log(nu/pi)+std::lgamma((nu-1)/2)-std::lgamma(nu/2));`. Execution-path causation remains open.

## Case 6930: st, interior

Data: `[0.11024753757609822, -0.049422603469292115, 1.3131208557689356, -0.22599451092730288, -1.3588404835226253, -1.554024854164954, -0.489892608032276, -1.5792660060875787, -3.481155288860334, 0.3740887816008062, 0.18182592636591727, -0.8722188835346512]`. Holds: `{}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6930` (run through the slot wrapper).
Returned: `{'xi': '0.62770644402967357', 'omega': '1.6458236548325134', 'alpha': '-2.5317285489909604', 'nu': '20.006809720088299'}`, status `0`, logLik `-18.50135385837926804697417537681758403778076171875`.
Independent logLik at that point: `-18.50135385837928589871765629` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.88e-07`.
Better feasible point: `[0.6277064207169055378450232543, 1.645823606053952403001326616, -2.531728433781182476228139998, 20.00680195880602563687040371]`; logLik `-18.50135385837928513053852839`.
Stationary local point: `[0.6277064207169055378450232543, 1.645823606053952403001326616, -2.531728433781182476228139998, 20.00680195880602563687040371]`; logLik `-18.50135385837928513053852839`. Verified coefficient relative error: `3.88e-07`.
Local information eigenvalue range: `[0.0007513841329760684802582689087, 21.60257246050715454754064281]`; per-observation stationarity residual `6.016861321697003884563369205e-49`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 6941: st, scale-1e-150

Data: `[1.1024753757609822e-151, -4.942260346929212e-152, 1.3131208557689357e-150, -2.259945109273029e-151, -1.3588404835226254e-150, -1.554024854164954e-150, -4.89892608032276e-151, -1.5792660060875789e-150, -3.481155288860334e-150, 3.7408878160080625e-151, 1.8182592636591727e-151, -8.722188835346512e-151]`. Holds: `{}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6941` (run through the slot wrapper).
Returned: `{'xi': '6.2770645763926485e-151', 'omega': '1.6458236783453717e-150', 'alpha': '-2.5317286073490779', 'nu': '20.006812215672657'}`, status `0`, logLik `4126.1518135309024728485383093357086181640625`.
Independent logLik at that point: `4126.15181353090294443739279` (231 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.13e-07`.
Better feasible point: `[6.277064207169058136888394574e-151, 1.645823606053952983427003597e-150, -2.531728433781183422566642822, 20.00680195880611009156102977]`; logLik `4126.15181353090294568440448`.
Stationary local point: `[6.277064207169058136888394574e-151, 1.645823606053952983427003597e-150, -2.531728433781183422566642822, 20.00680195880611009156102977]`; logLik `4126.15181353090294568440448`. Verified coefficient relative error: `5.13e-07`.
Local information eigenvalue range: `[0.0007513843167940370647105140394, 21.60257247427756305979312418]`; per-observation stationarity residual `1.500922693432678408383103868e-48`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 6942: st, scale-1e+150

Data: `[1.1024753757609823e+149, -4.9422603469292116e+148, 1.3131208557689355e+150, -2.2599451092730287e+149, -1.3588404835226253e+150, -1.554024854164954e+150, -4.89892608032276e+149, -1.5792660060875786e+150, -3.481155288860334e+150, 3.740887816008062e+149, 1.8182592636591726e+149, -8.722188835346512e+149]`. Holds: `{}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6942` (run through the slot wrapper).
Returned: `{'xi': '6.2770640579960545e+149', 'omega': '1.6458235766454428e+150', 'alpha': '-2.5317283425469026', 'nu': '20.006800547370389'}`, status `0`, logLik `-4163.154521247661250527016818523406982421875`.
Independent logLik at that point: `-4163.154521247661516847899037` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `7.05e-08`.
Better feasible point: `[6.277064207169055702507050797e+149, 1.645823606053952477776961986e+150, -2.531728433781182694933150287, 20.0068019588060463029492129]`; logLik `-4163.1545212476615159876587`.
Stationary local point: `[6.277064207169055702507050797e+149, 1.645823606053952477776961986e+150, -2.531728433781182694933150287, 20.0068019588060463029492129]`; logLik `-4163.1545212476615159876587`. Verified coefficient relative error: `7.05e-08`.
Local information eigenvalue range: `[0.0007513834574670057010906774484, 21.60257241407281711965480743]`; per-observation stationarity residual `1.529846505057510820716877348e-52`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 6943: st, offset-1e8

Data: `[100000000.11024754, 99999999.9505774, 100000001.31312086, 99999999.77400549, 99999998.64115952, 99999998.44597514, 99999999.5101074, 99999998.42073399, 99999996.51884471, 100000000.37408878, 100000000.18182592, 99999999.12778112]`. Holds: `{}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6943` (run through the slot wrapper).
Returned: `{'xi': '100000000.62770641', 'omega': '1.6458236223272922', 'alpha': '-2.5317284345725395', 'nu': '20.006803144274649'}`, status `0`, logLik `-18.501353861116033527878244058229029178619384765625`.
Independent logLik at that point: `-18.50135386111604364095922646` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.42e-07`.
Better feasible point: `[100000000.6277064129243783736, 1.645823594209132893258842752, -2.531728402460520800469487804, 20.00680030291566306278621656]`; logLik `-18.50135386111604160000136316`.
Stationary local point: `[100000000.6277064129243783736, 1.645823594209132893258842752, -2.531728402460520800469487804, 20.00680030291566306278621656]`; logLik `-18.50135386111604160000136316`. Verified coefficient relative error: `1.42e-07`.
Local information eigenvalue range: `[0.0007513838798046330624215643374, 21.60257246613492876896697395]`; per-observation stationarity residual `1.103845184533164542105505397e-52`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 6946: st, interior

Data: `[0.11024753757609822, -0.049422603469292115, 1.3131208557689356, -0.22599451092730288, -1.3588404835226253, -1.554024854164954, -0.489892608032276, -1.5792660060875787, -3.481155288860334, 0.3740887816008062, 0.18182592636591727, -0.8722188835346512]`. Holds: `{'xi': -0.036539023191020197}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6946` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '1.0429415947843461', 'alpha': '-0.72181776421283339', 'nu': '4.9567941288883031'}`, status `0`, logLik `-18.73023330279107767637469805777072906494140625`.
Independent logLik at that point: `-18.73023330279107920430144671` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.64e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 1.042941592032652247691744371, -0.7218177608503583751792685358, 4.956794047751128256007334707]`; logLik `-18.73023330279107912991149301`.
Stationary local point: `[-0.03653902319102019652374835346, 1.042941592032652247691744371, -0.7218177608503583751792685358, 4.956794047751128256007334707]`; logLik `-18.73023330279107912991149301`. Verified coefficient relative error: `1.64e-08`.
Local information eigenvalue range: `[0.03238568384398966401988634371, 1.345688334977746076315211556]`; per-observation stationarity residual `5.096545413147629872488873092e-61`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 6948: st, tiny-3

Data: `[0.11024753757609822, -0.049422603469292115, 1.3131208557689356]`. Holds: `{'xi': -0.036539023191020197}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6948` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '0.16483689150486855', 'alpha': '2.3259059569942964', 'nu': '1'}`, status `0`, logLik `-1.771558585637517779787231120280921459197998046875`.
Independent logLik at that point: `-1.771558585637518334526438025` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.31e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 0.1648368914686740245739223479, 2.3259060106295474590627193, 1.0]`; logLik `-1.771558585637518193631080311`.
Stationary local point: `[-0.03653902319102019652374835346, 0.1648368914686740245739223479, 2.3259060106295474590627193, 1.0]`; logLik `-1.771558585637518193631080311`. Verified coefficient relative error: `2.31e-08`.
Local information eigenvalue range: `[0.1386507493492319801498108692, 0.5087026900740823409414191776]`; per-observation stationarity residual `3.71272523872220912957153156e-61`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 6950: st, ties

Data: `[0.11024753757609822, 0.11024753757609822, 0.11024753757609822, 0.11024753757609822, -0.049422603469292115, 1.3131208557689356]`. Holds: `{'xi': -0.036539023191020197}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6950` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '0.16346135605836337', 'alpha': '3.4186182240984562', 'nu': '1.4018069345250785'}`, status `0`, logLik `0.504958061489518161835121645708568394184112548828125`.
Independent logLik at that point: `0.5049580614895152309000075108` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.68e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 0.163461356426947984379424278, 3.418618132444002977533657952, 1.401806914455761650042269984]`; logLik `0.5049580614895159390687120952`.
Stationary local point: `[-0.03653902319102019652374835346, 0.163461356426947984379424278, 3.418618132444002977533657952, 1.401806914455761650042269984]`; logLik `0.5049580614895159390687120952`. Verified coefficient relative error: `2.68e-08`.
Local information eigenvalue range: `[0.1359900612608616105339630266, 0.9546386589740810774647261203]`; per-observation stationarity residual `3.453151333216433978253792141e-60`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 6958: st, scale-1e+150

Data: `[1.1024753757609823e+149, -4.9422603469292116e+148, 1.3131208557689355e+150, -2.2599451092730287e+149, -1.3588404835226253e+150, -1.554024854164954e+150, -4.89892608032276e+149, -1.5792660060875786e+150, -3.481155288860334e+150, 3.740887816008062e+149, 1.8182592636591726e+149, -8.722188835346512e+149]`. Holds: `{'xi': -0.036539023191020197}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6958` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '1.0561083545242963e+150', 'alpha': '-0.78594354383128384', 'nu': '4.9416109165469475'}`, status `0`, logLik `-4163.363855652087295311503112316131591796875`.
Independent logLik at that point: `-4163.363855652087988621498081` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.16e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 1.056108351610690361772098963e+150, -0.7859435430053282716355879394, 4.941610859075583192184878604]`; logLik `-4163.363855652087988592142877`.
Stationary local point: `[-0.03653902319102019652374835346, 1.056108351610690361772098963e+150, -0.7859435430053282716355879394, 4.941610859075583192184878604]`; logLik `-4163.363855652087988592142877`. Verified coefficient relative error: `1.16e-08`.
Local information eigenvalue range: `[0.03193785491690395777217796929, 1.354663374219365136489494022]`; per-observation stationarity residual `8.737228178714387653693749229e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 6990: st, scale-1e+150

Data: `[1.1024753757609823e+149, -4.9422603469292116e+148, 1.3131208557689355e+150, -2.2599451092730287e+149, -1.3588404835226253e+150, -1.554024854164954e+150, -4.89892608032276e+149, -1.5792660060875786e+150, -3.481155288860334e+150, 3.740887816008062e+149, 1.8182592636591726e+149, -8.722188835346512e+149]`. Holds: `{'xi': -0.036539023191020197, 'omega': 0.9203517925594821}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6990` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '0.92035179255948207', 'alpha': '-0.35355337461585784', 'nu': '1'}`, status `0`, logLik `-8288.3321696338462061248719692230224609375`.
Independent logLik at that point: `-8288.332169633848482345329576` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.6e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 0.9203517925594820736634460445, -0.3535533905932737622004221811, 1.0]`; logLik `-8288.332169633848481135123606`.
Stationary local point: `[-0.03653902319102019652374835346, 0.9203517925594820736634460445, -0.3535533905932737622004221811, 1.0]`; logLik `-8288.332169633848481135123606`. Verified coefficient relative error: `1.6e-08`.
Local information eigenvalue range: `[0.09876542317215692739181602629, 0.09876542317215692739181602629]`; per-observation stationarity residual `2.192298313051066664800721058e-64`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 6994: st, interior

Data: `[0.11024753757609822, -0.049422603469292115, 1.3131208557689356, -0.22599451092730288, -1.3588404835226253, -1.554024854164954, -0.489892608032276, -1.5792660060875787, -3.481155288860334, 0.3740887816008062, 0.18182592636591727, -0.8722188835346512]`. Holds: `{'alpha': 0.10872382324228061}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 6994` (run through the slot wrapper).
Returned: `{'xi': '-0.63425477869659219', 'omega': '1.0187718421669545', 'alpha': '0.10872382324228061', 'nu': '6.8960232475481673'}`, status `0`, logLik `-19.02414861074195329138092347420752048492431640625`.
Independent logLik at that point: `-19.02414861074195591111002639` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.2e-08`.
Better feasible point: `[-0.6342547784178556066625396131, 1.018771834184135974470384506, 0.1087238232422806094579215141, 6.896022889206245056367242551]`; logLik `-19.02414861074195557614797569`.
Stationary local point: `[-0.6342547784178556066625396131, 1.018771834184135974470384506, 0.1087238232422806094579215141, 6.896022889206245056367242551]`; logLik `-19.02414861074195557614797569`. Verified coefficient relative error: `5.2e-08`.
Local information eigenvalue range: `[0.01615537434572992694862989358, 17.84475281927601227462673995]`; per-observation stationarity residual `6.977765449397720755451344805e-57`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7005: st, scale-1e-150

Data: `[1.1024753757609822e-151, -4.942260346929212e-152, 1.3131208557689357e-150, -2.259945109273029e-151, -1.3588404835226254e-150, -1.554024854164954e-150, -4.89892608032276e-151, -1.5792660060875789e-150, -3.481155288860334e-150, 3.7408878160080625e-151, 1.8182592636591727e-151, -8.722188835346512e-151]`. Holds: `{'alpha': 0.10872382324228061}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7005` (run through the slot wrapper).
Returned: `{'xi': '-6.3425477997514488e-151', 'omega': '1.0187718388990932e-150', 'alpha': '0.10872382324228061', 'nu': '6.8960230687393196'}`, status `0`, logLik `4125.629018778539830236695706844329833984375`.
Independent logLik at that point: `4125.629018778540275294725778` (231 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.6e-08`.
Better feasible point: `[-6.342547784178557269110200879e-151, 1.018771834184136163110403365e-150, 0.1087238232422806094579215141, 6.896022889206252221196759055]`; logLik `4125.629018778540275372755391`.
Stationary local point: `[-6.342547784178557269110200879e-151, 1.018771834184136163110403365e-150, 0.1087238232422806094579215141, 6.896022889206252221196759055]`; logLik `4125.629018778540275372755391`. Verified coefficient relative error: `2.6e-08`.
Local information eigenvalue range: `[0.01615537352181199581932781699, 17.84475281900268707960523751]`; per-observation stationarity residual `1.195823037799234136227863381e-59`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7006: st, scale-1e+150

Data: `[1.1024753757609823e+149, -4.9422603469292116e+148, 1.3131208557689355e+150, -2.2599451092730287e+149, -1.3588404835226253e+150, -1.554024854164954e+150, -4.89892608032276e+149, -1.5792660060875786e+150, -3.481155288860334e+150, 3.740887816008062e+149, 1.8182592636591726e+149, -8.722188835346512e+149]`. Holds: `{'alpha': 0.10872382324228061}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7006` (run through the slot wrapper).
Returned: `{'xi': '-6.3425477997532522e+149', 'omega': '1.0187718389027945e+150', 'alpha': '0.10872382324228061', 'nu': '6.8960230686563513'}`, status `0`, logLik `-4163.6773160000238931388594210147857666015625`.
Independent logLik at that point: `-4163.677316000024186460577778` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.6e-08`.
Better feasible point: `[-6.342547784178556210599636477e+149, 1.018771834184135977992326009e+150, 0.1087238232422806094579215141, 6.896022889206246782546172591]`; logLik `-4163.677316000024186382516705`.
Stationary local point: `[-6.342547784178556210599636477e+149, 1.018771834184135977992326009e+150, 0.1087238232422806094579215141, 6.896022889206246782546172591]`; logLik `-4163.677316000024186382516705`. Verified coefficient relative error: `2.6e-08`.
Local information eigenvalue range: `[0.01615537352143283320814270092, 17.84475281900285196348765721]`; per-observation stationarity residual `1.184547259553252090915131039e-59`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7007: st, offset-1e8

Data: `[100000000.11024754, 99999999.9505774, 100000001.31312086, 99999999.77400549, 99999998.64115952, 99999998.44597514, 99999999.5101074, 99999998.42073399, 99999996.51884471, 100000000.37408878, 100000000.18182592, 99999999.12778112]`. Holds: `{'alpha': 0.10872382324228061}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7007` (run through the slot wrapper).
Returned: `{'xi': '99999999.365745217', 'omega': '1.0187718408345989', 'alpha': '0.10872382324228061', 'nu': '6.8960231763478186'}`, status `0`, logLik `-19.02414861434260018313580076210200786590576171875`.
Independent logLik at that point: `-19.02414861434260154493547506` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.19e-08`.
Better feasible point: `[99999999.36574522193689612522, 1.018771832858633361491882952, 0.1087238232422806094579215141, 6.896022818345716056052654354]`; logLik `-19.02414861434260126540579693`.
Stationary local point: `[99999999.36574522193689612522, 1.018771832858633361491882952, 0.1087238232422806094579215141, 6.896022818345716056052654354]`; logLik `-19.02414861434260126540579693`. Verified coefficient relative error: `5.19e-08`.
Local information eigenvalue range: `[0.01615537459370958808573517436, 17.84475285985347253046577925]`; per-observation stationarity residual `4.70050159161905309286519941e-57`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7012: st, tiny-3

Data: `[0.11024753757609822, -0.049422603469292115, 1.3131208557689356]`. Holds: `{'xi': -0.036539023191020197, 'alpha': 0.10872382324228061}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7012` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '0.14220976351407261', 'alpha': '0.10872382324228061', 'nu': '1'}`, status `0`, logLik `-2.6607683118653699949618385289795696735382080078125`.
Independent logLik at that point: `-2.660768311865370137783750141` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `8.02e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 0.1422097521136596318223147721, 0.1087238232422806094579215141, 1.0]`; logLik `-2.660768311865366580552963061`.
Stationary local point: `[-0.03653902319102019652374835346, 0.1422097521136596318223147721, 0.1087238232422806094579215141, 1.0]`; logLik `-2.660768311865366580552963061`. Verified coefficient relative error: `8.02e-08`.
Local information eigenvalue range: `[0.3690102915765257141211878606, 0.3690102915765257141211878606]`; per-observation stationarity residual `9.872787438913800821966280141e-57`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7014: st, ties

Data: `[0.11024753757609822, 0.11024753757609822, 0.11024753757609822, 0.11024753757609822, -0.049422603469292115, 1.3131208557689356]`. Holds: `{'xi': -0.036539023191020197, 'alpha': 0.10872382324228061}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7014` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '0.15030930898879305', 'alpha': '0.10872382324228061', 'nu': '1.2845767808500903'}`, status `0`, logLik `-2.124932656501773209356542793102562427520751953125`.
Independent logLik at that point: `-2.124932656501774864154051995` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.78e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 0.1503093073140246855657816708, 0.1087238232422806094579215141, 1.284576758013911792634244184]`; logLik `-2.12493265650177440232095801`.
Stationary local point: `[-0.03653902319102019652374835346, 0.1503093073140246855657816708, 0.1087238232422806094579215141, 1.284576758013911792634244184]`; logLik `-2.12493265650177440232095801`. Verified coefficient relative error: `1.78e-08`.
Local information eigenvalue range: `[0.3363727506753973792985733628, 0.8486801355765411151742480902]`; per-observation stationarity residual `6.24316702261478000024323024e-62`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7022: st, scale-1e+150

Data: `[1.1024753757609823e+149, -4.9422603469292116e+148, 1.3131208557689355e+150, -2.2599451092730287e+149, -1.3588404835226253e+150, -1.554024854164954e+150, -4.89892608032276e+149, -1.5792660060875786e+150, -3.481155288860334e+150, 3.740887816008062e+149, 1.8182592636591726e+149, -8.722188835346512e+149]`. Holds: `{'xi': -0.036539023191020197, 'alpha': 0.10872382324228061}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7022` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '1.0035280681819158e+150', 'alpha': '0.10872382324228061', 'nu': '3.6644542258886879'}`, status `0`, logLik `-4165.3801428590304567478597164154052734375`.
Independent logLik at that point: `-4165.380142859030531572096099` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.07e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 1.00352805286947951435838526e+150, 0.1087238232422806094579215141, 3.664454113351907816446519738]`; logLik `-4165.38014285903053091884018`.
Stationary local point: `[-0.03653902319102019652374835346, 1.00352805286947951435838526e+150, 0.1087238232422806094579215141, 3.664454113351907816446519738]`; logLik `-4165.38014285903053091884018`. Verified coefficient relative error: `3.07e-08`.
Local information eigenvalue range: `[0.04276901104765556879563517311, 1.116888037624710160896567063]`; per-observation stationarity residual `1.934689250658090574886912896e-62`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7076: st, tiny-3

Data: `[0.11024753757609822, -0.049422603469292115, 1.3131208557689356]`. Holds: `{'xi': -0.036539023191020197, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7076` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '0.61124911332026788', 'alpha': '7.3046081799507476', 'nu': '4.8315112533735256'}`, status `0`, logLik `-2.30881221429061067595966960652731359004974365234375`.
Independent logLik at that point: `-2.308812214290610980371910667` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.19e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 0.6112490999514172372175367898, 7.304608148545446940856432574, 4.831511253373525605070426536]`; logLik `-2.308812214290610185323757062`.
Stationary local point: `[-0.03653902319102019652374835346, 0.6112490999514172372175367898, 7.304608148545446940856432574, 4.831511253373525605070426536]`; logLik `-2.308812214290610185323757062`. Verified coefficient relative error: `2.19e-08`.
Local information eigenvalue range: `[0.122101688455484359472809233, 1.176501706757137904191427308]`; per-observation stationarity residual `1.513275496926658150793495637e-60`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7077: st, tiny-5

Data: `[0.11024753757609822, -0.049422603469292115, 1.3131208557689356, -0.22599451092730288, -1.3588404835226253]`. Holds: `{'xi': -0.036539023191020197, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7077` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '0.71813699241316231', 'alpha': '-0.017074105202430526', 'nu': '4.8315112533735256'}`, status `0`, logLik `-6.4128470612198924527547205798327922821044921875`.
Independent logLik at that point: `-6.412847061219894160356764815` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.09e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 0.7181369926221730133880924036, -0.01707413609590067953908275414, 4.831511253373525605070426536]`; logLik `-6.412847061219892765700453795`.
Stationary local point: `[-0.03653902319102019652374835346, 0.7181369926221730133880924036, -0.01707413609590067953908275414, 4.831511253373525605070426536]`; logLik `-6.412847061219892765700453795`. Verified coefficient relative error: `3.09e-08`.
Local information eigenvalue range: `[0.0001703613606704250463029107103, 1.186721202923715708212581771]`; per-observation stationarity residual `3.566155004348185877031161297e-62`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7078: st, ties

Data: `[0.11024753757609822, 0.11024753757609822, 0.11024753757609822, 0.11024753757609822, -0.049422603469292115, 1.3131208557689356]`. Holds: `{'xi': -0.036539023191020197, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7078` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '0.28687551105415482', 'alpha': '5.1537795426812512', 'nu': '4.8315112533735256'}`, status `0`, logLik `-0.770169723781998438738582990481518208980560302734375`.
Independent logLik at that point: `-0.7701697237819983729781422545` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.41e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 0.28687551509189646572188355, 5.153779572351393295433425879, 4.831511253373525605070426536]`; logLik `-0.7701697237819979457545426141`.
Stationary local point: `[-0.03653902319102019652374835346, 0.28687551509189646572188355, 5.153779572351393295433425879, 4.831511253373525605070426536]`; logLik `-0.7701697237819979457545426141`. Verified coefficient relative error: `1.41e-08`.
Local information eigenvalue range: `[0.1260878486903443148358551162, 0.852904007567083946657570115]`; per-observation stationarity residual `6.141990087724790955249622266e-62`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7086: st, scale-1e+150

Data: `[1.1024753757609823e+149, -4.9422603469292116e+148, 1.3131208557689355e+150, -2.2599451092730287e+149, -1.3588404835226253e+150, -1.554024854164954e+150, -4.89892608032276e+149, -1.5792660060875786e+150, -3.481155288860334e+150, 3.740887816008062e+149, 1.8182592636591726e+149, -8.722188835346512e+149]`. Holds: `{'xi': -0.036539023191020197, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7086` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '1.0516196860667561e+150', 'alpha': '-0.78540581143877508', 'nu': '4.8315112533735256'}`, status `0`, logLik `-4163.363958054242175421677529811859130859375`.
Independent logLik at that point: `-4163.363958054242469003848331` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.32e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 1.051619695474406190694478829e+150, -0.7854058246853261722396567491, 4.831511253373525605070426536]`; logLik `-4163.363958054242468316791441`.
Stationary local point: `[-0.03653902319102019652374835346, 1.051619695474406190694478829e+150, -0.7854058246853261722396567491, 4.831511253373525605070426536]`; logLik `-4163.363958054242468316791441`. Verified coefficient relative error: `1.32e-08`.
Local information eigenvalue range: `[0.1653613260523909491733934827, 1.299087262810304799832322762]`; per-observation stationarity residual `5.898348158816394766850242787e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7093: st, tiny-5

Data: `[0.11024753757609822, -0.049422603469292115, 1.3131208557689356, -0.22599451092730288, -1.3588404835226253]`. Holds: `{'omega': 0.9203517925594821, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7093` (run through the slot wrapper).
Returned: `{'xi': '-0.56292209109814584', 'omega': '0.92035179255948207', 'alpha': '0.89796623102587025', 'nu': '4.8315112533735256'}`, status `0`, logLik `-6.46708696461662935206504698726348578929901123046875`.
Independent logLik at that point: `-6.467086964616629337822609043` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.18e-08`.
Better feasible point: `[-0.5629220950933349777900125245, 0.9203517925594820736634460445, 0.8979662428652200170456676113, 4.831511253373525605070426536]`; logLik `-6.467086964616629290632625444`.
Stationary local point: `[-0.5629220950933349777900125245, 0.9203517925594820736634460445, 0.8979662428652200170456676113, 4.831511253373525605070426536]`; logLik `-6.467086964616629290632625444`. Verified coefficient relative error: `1.18e-08`.
Local information eigenvalue range: `[0.1072033618546407288819657616, 9.009867284088494411031441619]`; per-observation stationarity residual `1.771857622607594463301480167e-65`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7122: st, interior

Data: `[0.11024753757609822, -0.049422603469292115, 1.3131208557689356, -0.22599451092730288, -1.3588404835226253, -1.554024854164954, -0.489892608032276, -1.5792660060875787, -3.481155288860334, 0.3740887816008062, 0.18182592636591727, -0.8722188835346512]`. Holds: `{'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7122` (run through the slot wrapper).
Returned: `{'xi': '-0.60645730889521587', 'omega': '0.96562308503403094', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `-19.041869254621733631438473821617662906646728515625`.
Independent logLik at that point: `-19.04186925462173958329382687` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.28e-08`.
Better feasible point: `[-0.6064573070375925141359303298, 0.965623063041712340225441626, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-19.04186925462173568981092923`.
Stationary local point: `[-0.6064573070375925141359303298, 0.965623063041712340225441626, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-19.04186925462173568981092923`. Verified coefficient relative error: `2.28e-08`.
Local information eigenvalue range: `[1.242141910724605983478850449, 18.36244995305009487928753744]`; per-observation stationarity residual `8.870488903419006195481571169e-60`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7124: st, tiny-3

Data: `[0.11024753757609822, -0.049422603469292115, 1.3131208557689356]`. Holds: `{'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7124` (run through the slot wrapper).
Returned: `{'xi': '0.29532259672477135', 'omega': '0.55128115591261839', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `-2.941635239109845034732870772131718695163726806640625`.
Independent logLik at that point: `-2.941635239109846134093708497` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.03e-08`.
Better feasible point: `[0.295322586459609719092620893, 0.5512811536517844269085850105, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-2.941635239109845773126952858`.
Stationary local point: `[0.295322586459609719092620893, 0.5512811536517844269085850105, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-2.941635239109845773126952858`. Verified coefficient relative error: `1.03e-08`.
Local information eigenvalue range: `[1.23315048961426624949107227, 4.552679346539107620515300065]`; per-observation stationarity residual `1.177612553412914725559765753e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7125: st, tiny-5

Data: `[0.11024753757609822, -0.049422603469292115, 1.3131208557689356, -0.22599451092730288, -1.3588404835226253]`. Holds: `{'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7125` (run through the slot wrapper).
Returned: `{'xi': '-0.11980216639254054', 'omega': '0.72008492643434907', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `-6.41046765311451327562508595292456448078155517578125`.
Independent logLik at that point: `-6.410467653114513723338649079` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.8e-08`.
Better feasible point: `[-0.1198021844296284637034216706, 0.7200849266973893280223640029, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-6.41046765311451253318245016`.
Stationary local point: `[-0.1198021844296284637034216706, 0.7200849266973893280223640029, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-6.41046765311451253318245016`. Verified coefficient relative error: `1.8e-08`.
Local information eigenvalue range: `[1.186261257120549790755878688, 10.46561754783701407811558803]`; per-observation stationarity residual `5.361613951762265406795981773e-62`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7126: st, ties

Data: `[0.11024753757609822, 0.11024753757609822, 0.11024753757609822, 0.11024753757609822, -0.049422603469292115, 1.3131208557689356]`. Holds: `{'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7126` (run through the slot wrapper).
Returned: `{'xi': '0.091364103532195054', 'omega': '0.17981394546106336', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `-2.63182559698847651219466570182703435420989990234375`.
Independent logLik at that point: `-2.631825596988480807958386862` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.31e-08`.
Better feasible point: `[0.09136410079724262711301732588, 0.1798139359046003241868120386, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-2.631825596988477507651420368`.
Stationary local point: `[0.09136410079724262711301732588, 0.1798139359046003241868120386, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-2.631825596988477507651420368`. Verified coefficient relative error: `5.31e-08`.
Local information eigenvalue range: `[0.3677000399067657675775413931, 53.22725690799345405183206223]`; per-observation stationarity residual `1.005440757200025390990138016e-56`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7127: st, constant--0.225995-0.0001

Data: `[-0.22603970982948834, -0.2260171103783956, -0.22599451092730288, -0.22597191147621015, -0.2259267125740247]`. Holds: `{'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7127` (run through the slot wrapper).
Returned: `{'xi': '-0.2259963890579148', 'omega': '3.4888000097512861e-05', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `43.4452434956654514053298044018447399139404296875`.
Independent logLik at that point: `43.44524349566544632878011641` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.29e-08`.
Better feasible point: `[-0.2259963890608161072890774094, 0.00003488799964809810018050084745, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `43.44524349566545916758271503`.
Stationary local point: `[-0.2259963890608161072890774094, 0.00003488799964809810018050084745, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `43.44524349566545916758271503`. Verified coefficient relative error: `1.29e-08`.
Local information eigenvalue range: `[1.359520889164962835462985309, 7.591291409405996080246552219]`; per-observation stationarity residual `4.008218741930177773074079306e-58`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7128: st, constant--0.225995-1e-08

Data: `[-0.2259945154471931, -0.225994513187248, -0.22599451092730288, -0.22599450866735776, -0.22599450414746755]`. Holds: `{'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7128` (run through the slot wrapper).
Returned: `{'xi': '-0.22599451111511595', 'omega': '3.4888000132059871e-09', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `89.4969453517811501797041273675858974456787109375`.
Independent logLik at that point: `89.49694535178114924783003045` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.29e-08`.
Better feasible point: `[-0.2259945111151162332261713656, 3.488799968264511685924529755e-9, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `89.49694535178116120742696956`.
Stationary local point: `[-0.2259945111151162332261713656, 3.488799968264511685924529755e-9, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `89.49694535178116120742696956`. Verified coefficient relative error: `1.29e-08`.
Local information eigenvalue range: `[1.359520890231349047542564292, 7.591291388991554818942929639]`; per-observation stationarity residual `3.089177396905513087020072216e-58`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7129: st, constant--0.225995-1e-12

Data: `[-0.22599451092775488, -0.22599451092752887, -0.22599451092730288, -0.2259945109270769, -0.2259945109266249]`. Holds: `{'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7129` (run through the slot wrapper).
Returned: `{'xi': '-0.22599451092732167', 'omega': '3.4887970155922188e-13', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `135.5486396471964098964235745370388031005859375`.
Independent logLik at that point: `135.5486396462470210689788339` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.29e-08`.
Better feasible point: `[-0.2259945109273216654611720767, 3.488796970648078877148744496e-13, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `135.5486396471964093482627275`.
Stationary local point: `[-0.2259945109273216654611720767, 3.488796970648078877148744496e-13, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `135.5486396471964093482627275`. Verified coefficient relative error: `1.29e-08`.
Local information eigenvalue range: `[1.359510225143757259941878542, 7.591495554102310838009444953]`; per-observation stationarity residual `3.341805047925972655436969203e-58`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7130: st, constant-1-0.0001

Data: `[0.9998, 0.9999, 1.0, 1.0001, 1.0003]`. Holds: `{'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7130` (run through the slot wrapper).
Returned: `{'xi': '0.99999168948571271', 'omega': '0.00015437543130740437', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `36.0090206559806205177665106020867824554443359375`.
Independent logLik at that point: `36.00902065598062186212448479` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.29e-08`.
Better feasible point: `[0.9999916894728747271901555476, 0.0001543754293187951722718164841, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `36.0090206559806347010123998`.
Stationary local point: `[0.9999916894728747271901555476, 0.0001543754293187951722718164841, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `36.0090206559806347010123998`. Verified coefficient relative error: `1.29e-08`.
Local information eigenvalue range: `[1.359520889164962982965756828, 7.591291409405996080666800382]`; per-observation stationarity residual `4.008316851987012324971482537e-58`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7131: st, constant-1-1e-08

Data: `[0.99999998, 0.99999999, 1.0, 1.00000001, 1.00000003]`. Holds: `{'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7131` (run through the slot wrapper).
Returned: `{'xi': '0.99999999916894855', 'omega': '1.5437543133436927e-08', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `82.0607225136435545209678821265697479248046875`.
Independent logLik at that point: `82.06072251364357065373705151` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.29e-08`.
Better feasible point: `[0.9999999991689472610739813311, 0.00000001543754293457600822155191245, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `82.06072251364358359414987747`.
Stationary local point: `[0.9999999991689472610739813311, 0.00000001543754293457600822155191245, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `82.06072251364358359414987747`. Verified coefficient relative error: `1.29e-08`.
Local information eigenvalue range: `[1.359520888037524863275196636, 7.591291417931983763528820698]`; per-observation stationarity residual `4.126339265611301952348695293e-58`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7132: st, constant-1-1e-12

Data: `[0.999999999998, 0.999999999999, 1.0, 1.000000000001, 1.000000000003]`. Holds: `{'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7132` (run through the slot wrapper).
Returned: `{'xi': '0.99999999999991696', 'omega': '1.5437669750007812e-12', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `128.112392389445858498220331966876983642578125`.
Independent logLik at that point: `128.1123923889794838312595354` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.29e-08`.
Better feasible point: `[0.9999999999999169305244296906, 1.543766955108783794508766077e-12, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `128.112392389445878735926374`.
Stationary local point: `[0.9999999999999169305244296906, 1.543766955108783794508766077e-12, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `128.112392389445878735926374`. Verified coefficient relative error: `1.29e-08`.
Local information eigenvalue range: `[1.359528266109674705394163487, 7.591151207008540876352752058]`; per-observation stationarity residual `2.77751124791403587529512611e-59`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7133: st, scale-1e-150

Data: `[1.1024753757609822e-151, -4.942260346929212e-152, 1.3131208557689357e-150, -2.259945109273029e-151, -1.3588404835226254e-150, -1.554024854164954e-150, -4.89892608032276e-151, -1.5792660060875789e-150, -3.481155288860334e-150, 3.7408878160080625e-151, 1.8182592636591727e-151, -8.722188835346512e-151]`. Holds: `{'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7133` (run through the slot wrapper).
Returned: `{'xi': '-6.0645728978959296e-151', 'omega': '9.6562307648383632e-151', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `4125.6112981346605010912753641605377197265625`.
Independent logLik at that point: `4125.611298134660491877141961` (231 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.39e-08`.
Better feasible point: `[-6.064573070375925611637683804e-151, 9.656230630417123927692490225e-151, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `4125.611298134660495179997716`.
Stationary local point: `[-6.064573070375925611637683804e-151, 9.656230630417123927692490225e-151, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `4125.611298134660495179997716`. Verified coefficient relative error: `1.39e-08`.
Local information eigenvalue range: `[1.242141888761254912105640862, 18.36244995254808413805146029]`; per-observation stationarity residual `1.212328287471909111522738027e-60`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7134: st, scale-1e+150

Data: `[1.1024753757609823e+149, -4.9422603469292116e+148, 1.3131208557689355e+150, -2.2599451092730287e+149, -1.3588404835226253e+150, -1.554024854164954e+150, -4.89892608032276e+149, -1.5792660060875786e+150, -3.481155288860334e+150, 3.740887816008062e+149, 1.8182592636591726e+149, -8.722188835346512e+149]`. Holds: `{'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7134` (run through the slot wrapper).
Returned: `{'xi': '-6.0645726431542814e+149', 'omega': '9.6562306508357696e+149', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `-4163.695036643903222284279763698577880859375`.
Independent logLik at that point: `-4163.695036643903975438203113` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `7.04e-08`.
Better feasible point: `[-6.064573070375925135611106708e+149, 9.656230630417123137966469861e+149, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-4163.695036643903966516103234`.
Stationary local point: `[-6.064573070375925135611106708e+149, 9.656230630417123137966469861e+149, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-4163.695036643903966516103234`. Verified coefficient relative error: `7.04e-08`.
Local information eigenvalue range: `[1.24214185947678771361461153, 18.36244995187874239430154448]`; per-observation stationarity residual `4.077124438976561046782992642e-60`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7135: st, offset-1e8

Data: `[100000000.11024754, 99999999.9505774, 100000001.31312086, 99999999.77400549, 99999998.64115952, 99999998.44597514, 99999999.5101074, 99999998.42073399, 99999996.51884471, 100000000.37408878, 100000000.18182592, 99999999.12778112]`. Holds: `{'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7135` (run through the slot wrapper).
Returned: `{'xi': '99999999.393542707', 'omega': '0.96562307635694999', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `-19.041869257280179539293385460041463375091552734375`.
Independent logLik at that point: `-19.04186925728017672865764816` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.39e-08`.
Better feasible point: `[99999999.39354269285195587631, 0.9656230629148264587705772416, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-19.04186925728017396957895639`.
Stationary local point: `[99999999.39354269285195587631, 0.9656230629148264587705772416, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-19.04186925728017396957895639`. Verified coefficient relative error: `1.39e-08`.
Local information eigenvalue range: `[1.242141887127249607467785625, 18.3624499905865630466163978]`; per-observation stationarity residual `7.929055659890097061802549376e-61`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7136: st, limit-outlier

Data: `[0.11024753757609822, -0.049422603469292115, 1.3131208557689356, -0.22599451092730288, 1000001.3131208557]`. Holds: `{'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7136` (run through the slot wrapper).
Returned: `{'xi': '23990.207017284047', 'omega': '193107.14748546318', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `-70.982869182836935806335532106459140777587890625`.
Independent logLik at that point: `-70.98286918283693654434073475` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.12e-07`.
Better feasible point: `[23990.20192217001927298379689, 193107.1468059966746049810769, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-70.98286918283693502734654789`.
Stationary local point: `[23990.20192217001927298379689, 193107.1468059966746049810769, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-70.98286918283693502734654789`. Verified coefficient relative error: `2.12e-07`.
Local information eigenvalue range: `[0.2909759551488666229166633878, 25.05037382087258372126517884]`; per-observation stationarity residual `5.944278639742446863263022846e-59`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7139: st, tiny-2

Data: `[0.11024753757609822, -0.049422603469292115]`. Holds: `{'xi': -0.036539023191020197, 'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7139` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '0.091342892920185548', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `1.68363869980291003258798809838481247425079345703125`.
Independent logLik at that point: `1.6836386998029096047212483` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.22e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 0.09134289403700399648406953937, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `1.683638699802909807802210918`.
Stationary local point: `[-0.03653902319102019652374835346, 0.09134289403700399648406953937, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `1.683638699802909807802210918`. Verified coefficient relative error: `1.22e-08`.
Local information eigenvalue range: `[1.358481010355863566414032838, 1.358481010355863566414032838]`; per-observation stationarity residual `4.702879768238296616172940173e-62`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7140: st, tiny-3

Data: `[0.11024753757609822, -0.049422603469292115, 1.3131208557689356]`. Holds: `{'xi': -0.036539023191020197, 'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7140` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '0.59521982226531545', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `-3.350471072572680508727671622182242572307586669921875`.
Independent logLik at that point: `-3.350471072572680103991281253` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.84e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 0.5952198570002977948449992172, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-3.350471072572674865669401568`.
Stationary local point: `[-0.03653902319102019652374835346, 0.5952198570002977948449992172, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-3.350471072572674865669401568`. Verified coefficient relative error: `5.84e-08`.
Local information eigenvalue range: `[1.025466560207092855924480162, 1.025466560207092855924480162]`; per-observation stationarity residual `2.636520038347463871639295908e-57`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7142: st, ties

Data: `[0.11024753757609822, 0.11024753757609822, 0.11024753757609822, 0.11024753757609822, -0.049422603469292115, 1.3131208557689356]`. Holds: `{'xi': -0.036539023191020197, 'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7142` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '0.27277870694000639', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `-3.61552671014481941114127039327286183834075927734375`.
Independent logLik at that point: `-3.615526710144821591551713014` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.41e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 0.2727787107799700932339056474, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-3.615526710144821194442526827`.
Stationary local point: `[-0.03653902319102019652374835346, 0.2727787107799700932339056474, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-3.615526710144821194442526827`. Verified coefficient relative error: `1.41e-08`.
Local information eigenvalue range: `[0.6679675506703801688645762953, 0.6679675506703801688645762953]`; per-observation stationarity residual `5.690405204649145182974432455e-62`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7144: st, constant--0.225995-1e-08

Data: `[-0.2259945154471931, -0.225994513187248, -0.22599451092730288, -0.22599450866735776, -0.22599450414746755]`. Holds: `{'xi': -0.036539023191020197, 'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7144` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '0.19821597643261038', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `0.29930945563324939939775504171848297119140625`.
Independent logLik at that point: `0.2993094556332418147745680424` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `9.71e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 0.1982159571770584279315094375, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `0.2993094556332796150728106215`.
Stationary local point: `[-0.03653902319102019652374835346, 0.1982159571770584279315094375, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `0.2993094556332796150728106215`. Verified coefficient relative error: `9.71e-08`.
Local information eigenvalue range: `[1.602214459137788432855191967, 1.602214459137788432855191967]`; per-observation stationarity residual `2.734412692941549579758933323e-54`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7145: st, constant--0.225995-1e-12

Data: `[-0.22599451092775488, -0.22599451092752887, -0.22599451092730288, -0.2259945109270769, -0.2259945109266249]`. Holds: `{'xi': -0.036539023191020197, 'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7145` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '0.19821595112742141', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `0.299309443705823241543839685618877410888671875`.
Independent logLik at that point: `0.2993094437058349931185593754` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.29e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 0.19821595764990023931506136, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `0.2993094437058393303112577966`.
Stationary local point: `[-0.03653902319102019652374835346, 0.19821595764990023931506136, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `0.2993094437058393303112577966`. Verified coefficient relative error: `3.29e-08`.
Local information eigenvalue range: `[1.602214042401139379000332511, 1.602214042401139379000332511]`; per-observation stationarity residual `4.739334427749858394210724656e-58`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7146: st, constant-1-0.0001

Data: `[0.9998, 0.9999, 1.0, 1.0001, 1.0003]`. Holds: `{'xi': -0.036539023191020197, 'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7146` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '0.99816329984436958', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `-7.3693146101760476085473783314228057861328125`.
Independent logLik at that point: `-7.369314610176047191034824844` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.88e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 0.9981632811132344423647655748, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-7.369314610176045680556840761`.
Stationary local point: `[-0.03653902319102019652374835346, 0.9981632811132344423647655748, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-7.369314610176045680556840761`. Verified coefficient relative error: `1.88e-08`.
Local information eigenvalue range: `[1.715731813495426373974718641, 1.715731813495426373974718641]`; per-observation stationarity residual `5.765327029370883640511688168e-60`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7149: st, scale-1e-150

Data: `[1.1024753757609822e-151, -4.942260346929212e-152, 1.3131208557689357e-150, -2.259945109273029e-151, -1.3588404835226254e-150, -1.554024854164954e-150, -4.89892608032276e-151, -1.5792660060875789e-150, -3.481155288860334e-150, 3.7408878160080625e-151, 1.8182592636591727e-151, -8.722188835346512e-151]`. Holds: `{'xi': -0.036539023191020197, 'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7149` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '0.035185557545624281', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `22.4570177374598642927594482898712158203125`.
Independent logLik at that point: `22.45701773745982966683913191` (231 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.09e-07`.
Better feasible point: `[-0.03653902319102019652374835346, 0.0351855613636089910891756988, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `22.45701773745995087730332518`.
Stationary local point: `[-0.03653902319102019652374835346, 0.0351855613636089910891756988, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `22.45701773745995087730332518`. Verified coefficient relative error: `1.09e-07`.
Local information eigenvalue range: `[1.715731405875137668827038862, 1.715731405875137668827038862]`; per-observation stationarity residual `7.205783068689883966977663015e-54`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7151: st, offset-1e8

Data: `[100000000.11024754, 99999999.9505774, 100000001.31312086, 99999999.77400549, 99999998.64115952, 99999998.44597514, 99999999.5101074, 99999998.42073399, 99999996.51884471, 100000000.37408878, 100000000.18182592, 99999999.12778112]`. Holds: `{'xi': -0.036539023191020197, 'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7151` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '96295840.552816778', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `-238.3036446498393843285157345235347747802734375`.
Independent logLik at that point: `-238.3036446498393737378716388` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.66e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 96295845.044227065484251318, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-238.3036446498393513428732199`.
Stationary local point: `[-0.03653902319102019652374835346, 96295845.044227065484251318, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-238.3036446498393513428732199`. Verified coefficient relative error: `4.66e-08`.
Local information eigenvalue range: `[1.715731618173585503618366057, 1.715731618173585503618366057]`; per-observation stationarity residual `8.397020117981103761416235073e-57`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7152: st, limit-outlier

Data: `[0.11024753757609822, -0.049422603469292115, 1.3131208557689356, -0.22599451092730288, 1000001.3131208557]`. Holds: `{'xi': -0.036539023191020197, 'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7152` (run through the slot wrapper).
Returned: `{'xi': '-0.036539023191020197', 'omega': '182770.88008684991', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `-71.016852766798621132693369872868061065673828125`.
Independent logLik at that point: `-71.01685276679862044119586855` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.46e-08`.
Better feasible point: `[-0.03653902319102019652374835346, 182770.8755987231593727872325, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-71.01685276679862001016437764`.
Stationary local point: `[-0.03653902319102019652374835346, 182770.8755987231593727872325, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-71.01685276679862001016437764`. Verified coefficient relative error: `2.46e-08`.
Local information eigenvalue range: `[0.2859254260104985769462932638, 0.2859254260104985769462932638]`; per-observation stationarity residual `6.691719417050409480940969239e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7155: st, tiny-2

Data: `[0.11024753757609822, -0.049422603469292115]`. Holds: `{'omega': 0.9203517925594821, 'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7155` (run through the slot wrapper).
Returned: `{'xi': '-0.038869818800596881', 'omega': '0.92035179255948207', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `-1.77693961828964575033751316368579864501953125`.
Independent logLik at that point: `-1.776939618289645337885583515` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `9.46e-07`.
Better feasible point: `[-0.03887076438461313087306646295, 0.9203517925594820736634460445, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-1.776939618288368516678747896`.
Stationary local point: `[-0.03887076438461313087306646295, 0.9203517925594820736634460445, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-1.776939618288368516678747896`. Verified coefficient relative error: `9.46e-07`.
Local information eigenvalue range: `[0.03640635949793476911543180937, 0.03640635949793476911543180937]`; per-observation stationarity residual `1.002218100032523474507003194e-60`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7159: st, constant--0.225995-0.0001

Data: `[-0.22603970982948834, -0.2260171103783956, -0.22599451092730288, -0.22597191147621015, -0.2259267125740247]`. Holds: `{'omega': 0.9203517925594821, 'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7159` (run through the slot wrapper).
Returned: `{'xi': '-0.22599704465117726', 'omega': '0.92035179255948207', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `-4.4366485494726788374464376829564571380615234375`.
Independent logLik at that point: `-4.436648549472679509806480202` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.00386`.
Better feasible point: `[-0.2951092327764204622638446644, 0.9203517925594820736634460445, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-4.419505886791143158679919895`.
Stationary local point: `[-0.2951092327764204622638446644, 0.9203517925594820736634460445, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-4.419505886791143158679919895`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7160: st, constant--0.225995-1e-08

Data: `[-0.2259945154471931, -0.225994513187248, -0.22599451092730288, -0.22599450866735776, -0.22599450414746755]`. Holds: `{'omega': 0.9203517925594821, 'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7160` (run through the slot wrapper).
Returned: `{'xi': '-0.22599451092730302', 'omega': '0.92035179255948207', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `-4.4366520431918416988992248661816120147705078125`.
Independent logLik at that point: `-4.436652043191837001031201765` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.00386`.
Better feasible point: `[-0.2951137521758027909093718863, 0.9203517925594820736634460445, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-4.419505881368671177431868583`.
Stationary local point: `[-0.2951137521758027909093718863, 0.9203517925594820736634460445, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-4.419505881368671177431868583`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7162: st, constant-1-0.0001

Data: `[0.9998, 0.9999, 1.0, 1.0001, 1.0003]`. Holds: `{'omega': 0.9203517925594821, 'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7162` (run through the slot wrapper).
Returned: `{'xi': '0.99995041859523859', 'omega': '0.92035179255948207', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `-4.43661764518873269480536691844463348388671875`.
Independent logLik at that point: `-4.436617645188734688291441503` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.00386`.
Better feasible point: `[0.930900757538515084084520917, 0.9203517925594820736634460445, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-4.419505987538506516666435251`.
Stationary local point: `[0.930900757538515084084520917, 0.9203517925594820736634460445, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-4.419505987538506516666435251`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7163: st, constant-1-1e-08

Data: `[0.99999998, 0.99999999, 1.0, 1.00000001, 1.00000003]`. Holds: `{'omega': 0.9203517925594821, 'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7163` (run through the slot wrapper).
Returned: `{'xi': '0.99999999999999756', 'omega': '0.92035179255948207', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `-4.436652042423844477525562979280948638916015625`.
Independent logLik at that point: `-4.436652042423814535706822468` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.00386`.
Better feasible point: `[0.9308807602995110486628214581, 0.9203517925594820736634460445, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-4.419505881368672184906735364`.
Stationary local point: `[0.9308807602995110486628214581, 0.9203517925594820736634460445, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-4.419505881368672184906735364`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7165: st, scale-1e-150

Data: `[1.1024753757609822e-151, -4.942260346929212e-152, 1.3131208557689357e-150, -2.259945109273029e-151, -1.3588404835226254e-150, -1.554024854164954e-150, -4.89892608032276e-151, -1.5792660060875789e-150, -3.481155288860334e-150, 3.7408878160080625e-151, 1.8182592636591727e-151, -8.722188835346512e-151]`. Holds: `{'omega': 0.9203517925594821, 'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7165` (run through the slot wrapper).
Returned: `{'xi': '-2.259945109273029e-151', 'omega': '0.92035179255948207', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `-10.6479649042003075010143220424652099609375`.
Independent logLik at that point: `-10.64796490419860296111265615` (231 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.00301`.
Better feasible point: `[-0.03653902319102019652374835346, 0.9203517925594820736634460445, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-10.61595556084620558962193579`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7169: st, limit-concentration

Data: `[-0.22599405893828103, -0.22599428493279194, -0.22599451092730288, -0.2259947369218138, -0.22599496291632476]`. Holds: `{'omega': 0.9203517925594821, 'alpha': 0.10872382324228061, 'nu': 4.831511253373526}`.
Call: `route_st(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7169` (run through the slot wrapper).
Returned: `{'xi': '-0.22599451092811373', 'omega': '0.92035179255948207', 'alpha': '0.10872382324228061', 'nu': '4.8315112533735256'}`, status `0`, logLik `-4.436652043416046353740966878831386566162109375`.
Independent logLik at that point: `-4.436652043416048668088260755` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.00386`.
Better feasible point: `[-0.2951137526277944389319275235, 0.9203517925594820736634460445, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-4.419505881369037506549512618`.
Stationary local point: `[-0.2951137526277944389319275235, 0.9203517925594820736634460445, 0.1087238232422806094579215141, 4.831511253373525605070426536]`; logLik `-4.419505881369037506549512618`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/st/mle.hpp:64`: Inspect the held-coordinate gradient mapping and acceptance gate: independent feasible points improve the returned likelihood. This line is a path nomination, not a proved causal attribution.

## Case 7202: std, interior

Data: `[-0.15843991426166942, 1.0193854969762004, 0.4064456964106538, -0.027810669345589987, 0.3137308027350768, 0.18830867449095567, 1.4016586602655614, 1.1572769868562465, 0.41789263725968806, -0.22753546970232982, 0.24722640164018242, 0.37492774587183186]`. Holds: `{'mean': -0.0062603717426506856}`.
Call: `route_std(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7202` (run through the slot wrapper).
Returned: `{'mean': '-0.0062603717426506856', 'sd': '0.67439190362899071', 'nu': '5.8886859233906046'}`, status `0`, logLik `-11.923967385184283784838044084608554840087890625`.
Independent logLik at that point: `-11.9239673851842872051883994` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `8.7e-08`.
Better feasible point: `[-0.006260371742650685557285417104, 0.6743919086249592882320587524, 5.888685411355868634397849735]`; logLik `-11.92396738518428683170060899`.
Stationary local point: `[-0.006260371742650685557285417104, 0.6743919086249592882320587524, 5.888685411355868634397849735]`; logLik `-11.92396738518428683170060899`. Verified coefficient relative error: `8.7e-08`.
Local information eigenvalue range: `[0.008173380507839499376927383512, 1.281453363928164311073750682]`; per-observation stationarity residual `2.510736176359130906227650987e-54`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/std/mle.hpp:134` (special difference): `const double dpsi = cppunivariate::special::digamma(0.5 * nu1) -`. Execution-path causation remains open.

## Case 7205: std, tiny-5

Data: `[-0.15843991426166942, 1.0193854969762004, 0.4064456964106538, -0.027810669345589987, 0.3137308027350768]`. Holds: `{'mean': -0.0062603717426506856}`.
Call: `route_std(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7205` (run through the slot wrapper).
Returned: `{'mean': '-0.0062603717426506856', 'sd': '0.53357555687956881', 'nu': '5.9102760649516544'}`, status `0`, logLik `-3.798642331156307339057320859865285456180572509765625`.
Independent logLik at that point: `-3.798642331156309089272433967` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `7.19e-08`.
Better feasible point: `[-0.006260371742650685557285417104, 0.5335755601995287596403338557, 5.91027564014627487543629061]`; logLik `-3.798642331156308959831321381`.
Stationary local point: `[-0.006260371742650685557285417104, 0.5335755601995287596403338557, 5.91027564014627487543629061]`; logLik `-3.798642331156308959831321381`. Verified coefficient relative error: `7.19e-08`.
Local information eigenvalue range: `[0.009947187205465195824721470833, 1.28778067732405803605711769]`; per-observation stationarity residual `4.76603157015383462154199037e-55`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/std/mle.hpp:134` (special difference): `const double dpsi = cppunivariate::special::digamma(0.5 * nu1) -`. Execution-path causation remains open.

## Case 7206: std, ties

Data: `[-0.15843991426166942, -0.15843991426166942, -0.15843991426166942, -0.15843991426166942, 1.0193854969762004, 0.4064456964106538]`. Holds: `{'mean': -0.0062603717426506856}`.
Call: `route_std(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7206` (run through the slot wrapper).
Returned: `{'mean': '-0.0062603717426506856', 'sd': '0.73037922542206324', 'nu': '2.3364426845079977'}`, status `0`, logLik `-3.575885012220651137937466046423651278018951416015625`.
Independent logLik at that point: `-3.575885012220652402978724029` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.04e-08`.
Better feasible point: `[-0.006260371742650685557285417104, 0.7303792622591338195580395501, 2.336442640277308572496394474]`; logLik `-3.575885012220652273093112991`.
Stationary local point: `[-0.006260371742650685557285417104, 0.7303792622591338195580395501, 2.336442640277308572496394474]`; logLik `-3.575885012220652273093112991`. Verified coefficient relative error: `5.04e-08`.
Local information eigenvalue range: `[0.01469613675189422596231128838, 7.124115985118676835837321028]`; per-observation stationarity residual `8.242551639267316853888033824e-54`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/std/mle.hpp:134` (special difference): `const double dpsi = cppunivariate::special::digamma(0.5 * nu1) -`. Execution-path causation remains open.

## Case 7214: std, scale-1e+150

Data: `[-1.5843991426166943e+149, 1.0193854969762003e+150, 4.064456964106538e+149, -2.781066934558999e+148, 3.137308027350768e+149, 1.8830867449095566e+149, 1.4016586602655614e+150, 1.1572769868562464e+150, 4.178926372596881e+149, -2.2753546970232982e+149, 2.472264016401824e+149, 3.7492774587183186e+149]`. Holds: `{'mean': -0.0062603717426506856}`.
Call: `route_std(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7214` (run through the slot wrapper).
Returned: `{'mean': '-0.0062603717426506856', 'sd': '6.7413127160867277e+149', 'nu': '5.4438695031199913'}`, status `0`, logLik `-4156.49330733638271340169012546539306640625`.
Independent logLik at that point: `-4156.493307336383601482401757` (230 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.37e-06`.
Better feasible point: `[-0.006260371742650685557285417104, 6.741311748073869836616472883e+149, 5.443876940497403566256910003]`; logLik `-4156.493307336383488458872257`.
Stationary local point: `[-0.006260371742650685557285417104, 6.741311748073869836616472883e+149, 5.443876940497403566256910003]`; logLik `-4156.493307336383488458872257`. Verified coefficient relative error: `1.37e-06`.
Local information eigenvalue range: `[0.009981182303893991471600218899, 1.246558263930922855075169576]`; per-observation stationarity residual `1.421453218106882737282032775e-44`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/std/mle.hpp:134` (special difference): `const double dpsi = cppunivariate::special::digamma(0.5 * nu1) -`. Execution-path causation remains open.

## Case 7219: std, tiny-2

Data: `[-0.15843991426166942, 1.0193854969762004]`. Holds: `{'sd': 0.8395065634734545}`.
Call: `route_std(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7219` (run through the slot wrapper).
Returned: `{'mean': '0.43047279135726552', 'sd': '0.83950656347345454', 'nu': '9.8264123937022791'}`, status `0`, logLik `-1.97140855240856449626107860240153968334197998046875`.
Independent logLik at that point: `-1.971408552408565220075144475` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.25e-07`.
Better feasible point: `[0.4304727913572654884877266568, 0.8395065634734545367834357421, 9.826410179089905843241834855]`; logLik `-1.971408552408564606625753313`.
Stationary local point: `[0.4304727913572654884877266568, 0.8395065634734545367834357421, 9.826410179089905843241834855]`; logLik `-1.971408552408564606625753313`. Verified coefficient relative error: `2.25e-07`.
Local information eigenvalue range: `[0.01207740137562601719079648034, 2.258739981085526349503963923]`; per-observation stationarity residual `5.572312587181253574365255929e-52`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/std/mle.hpp:134` (special difference): `const double dpsi = cppunivariate::special::digamma(0.5 * nu1) -`. Execution-path causation remains open.

## Case 7232: std, limit-outlier

Data: `[-0.15843991426166942, 1.0193854969762004, 0.4064456964106538, -0.027810669345589987, 1000001.4016586603]`. Holds: `{'sd': 0.8395065634734545}`.
Call: `route_std(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7232` (run through the slot wrapper).
Returned: `{'mean': '0.079471025086663066', 'sd': '0.83950656347345454', 'nu': '2.134344337498232'}`, status `0`, logLik `-50.129025105394475758657790720462799072265625`.
Independent logLik at that point: `-50.12902510539448276973575016` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.97e-08`.
Better feasible point: `[0.079471005387418141687391781, 0.8395065634734545367834357421, 2.134344329725945146231250006]`; logLik `-50.12902510539447705487860335`.
Stationary local point: `[0.079471005387418141687391781, 0.8395065634734545367834357421, 2.134344329725945146231250006]`; logLik `-50.12902510539447705487860335`. Verified coefficient relative error: `1.97e-08`.
Local information eigenvalue range: `[116.9890540221288373925719489, 4961158098726.419995914306904]`; per-observation stationarity residual `2.090730305213355651651884776e-51`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/std/mle.hpp:134` (special difference): `const double dpsi = cppunivariate::special::digamma(0.5 * nu1) -`. Execution-path causation remains open.

## Case 7236: std, tiny-3

Data: `[-0.15843991426166942, 1.0193854969762004, 0.4064456964106538]`. Holds: `{'mean': -0.0062603717426506856, 'sd': 0.8395065634734545}`.
Call: `route_std(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7236` (run through the slot wrapper).
Returned: `{'mean': '-0.0062603717426506856', 'sd': '0.83950656347345454', 'nu': '5.2122200887327033'}`, status `0`, logLik `-3.0912816614288320948844557278789579868316650390625`.
Independent logLik at that point: `-3.09128166142883280226189921` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.37e-08`.
Better feasible point: `[-0.006260371742650685557285417104, 0.8395065634734545367834357421, 5.212219808660353163588701265]`; logLik `-3.091281661428832653023777387`.
Stationary local point: `[-0.006260371742650685557285417104, 0.8395065634734545367834357421, 5.212219808660353163588701265]`; logLik `-3.091281661428832653023777387`. Verified coefficient relative error: `5.37e-08`.
Local information eigenvalue range: `[0.03445828062966593711450826447, 0.03445828062966593711450826447]`; per-observation stationarity residual `6.988736242441578056773580215e-56`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/std/mle.hpp:134` (special difference): `const double dpsi = cppunivariate::special::digamma(0.5 * nu1) -`. Execution-path causation remains open.

## Case 7240: std, constant-0.374928-1e-08

Data: `[0.3749277383732769, 0.3749277421225544, 0.37492774587183186, 0.3749277496211093, 0.37492775711966425]`. Holds: `{'mean': -0.0062603717426506856, 'sd': 0.8395065634734545}`.
Call: `route_std(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7240` (run through the slot wrapper).
Returned: `{'mean': '-0.0062603717426506856', 'sd': '0.83950656347345454', 'nu': '2.6283252466029636'}`, status `0`, logLik `-3.18199990826321510439811390824615955352783203125`.
Independent logLik at that point: `-3.181999908263216460256827481` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.06e-08`.
Better feasible point: `[-0.006260371742650685557285417104, 0.8395065634734545367834357421, 2.628325274518367026696488415]`; logLik `-3.181999908263215627610710003`.
Stationary local point: `[-0.006260371742650685557285417104, 0.8395065634734545367834357421, 2.628325274518367026696488415]`; logLik `-3.181999908263215627610710003`. Verified coefficient relative error: `1.06e-08`.
Local information eigenvalue range: `[2.952506235601076594793619872, 2.952506235601076594793619872]`; per-observation stationarity residual `1.626551755081492899779040592e-57`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/std/mle.hpp:134` (special difference): `const double dpsi = cppunivariate::special::digamma(0.5 * nu1) -`. Execution-path causation remains open.

## Case 7550: tlambda, scale-1e+150

Data: `[4.6865731534620613e+148, -8.301962549719304e+149, -9.073943855660944e+149, -3.255798582292203e+150, -1.9014148045354534e+150, 1.9857321094082114e+149, 1.8440334139116988e+150, -2.9471142485550538e+150, 1.0519688286977576e+150, -1.0063055520033321e+150, 1.797647756513512e+150, 2.8748400651987167e+150]`. Holds: `{'mu': 0.04752470610558923}`.
Call: `route_tlambda(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7550` (run through the slot wrapper).
Returned: `{'mu': '0.047524706105589232', 'sigma': '3.2557985822922032e+150', 'lambda': '1'}`, status `0`, logLik `-4167.136184585021`.
Independent logLik at that point: `-inf` (230 digits).
Evidence: returned coefficients exclude an observation from support. Worst relative discrepancy: `inf`.
Exact support exclusion: `{'bound': 'lower', 'observation_index': 3, 'exact_bound': '-1832851660250569425374335270487772223971966678987431141817083843052005265910625976175309335444049266269238742691631670328352703264704563036233828574250794593191398537/562949953421312', 'exact_observation': '-3255798582292203181108787771492125216305109843139986377410870664753881815142168634081282148862700271985983744634352757302680287069848509127172470865920', 'exact_gap': '26754031088503/562949953421312', 'bound_expression': 'Piecewise((p_mu - p_sigma/p_lambda, p_lambda > 0), (-oo, True))', 'basis': 'exact binary64 rational support comparison'}`. This is a returned-coefficient feasibility defect; a one-ULP projection can be practically small even though the exact returned-point likelihood is zero.
Suspected cause / path: Boundary reconstruction/projection leaves the exact returned support short of the data; inspect `univariateML-cpp/include/cppunivariate/distributions/tlambda/mle.hpp:183`: On the uniform face, restored radius/scale and held location can define an exact endpoint just inside an observation. The likelihood recheck at lines 184–185 uses rounded comparisons and can miss a gap smaller than a data ULP. Other shape/profile witnesses need separate review.

## Case 7566: tlambda, scale-1e+150

Data: `[4.6865731534620613e+148, -8.301962549719304e+149, -9.073943855660944e+149, -3.255798582292203e+150, -1.9014148045354534e+150, 1.9857321094082114e+149, 1.8440334139116988e+150, -2.9471142485550538e+150, 1.0519688286977576e+150, -1.0063055520033321e+150, 1.797647756513512e+150, 2.8748400651987167e+150]`. Holds: `{'sigma': 1.0988151244928166}`.
Call: `route_tlambda(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7566` (run through the slot wrapper).
Returned: `{'mu': '-1.0063055520033321e+150', 'sigma': '1.0988151244928166', 'lambda': '-1'}`, status `0`, logLik `-7603.082859822521641035564243793487548828125`.
Independent logLik at that point: `-7603.082859822523647245394305` (230 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.000252`.
Better feasible point: `[-9.073943855660943616396717788e+149, 1.098815124492816597268074474, -1.0]`; logLik `-7601.16734678029790836861442`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/tlambda/mle.hpp:183`: On the uniform face, restored radius/scale and held location can define an exact endpoint just inside an observation. The likelihood recheck at lines 184–185 uses rounded comparisons and can miss a gap smaller than a data ULP. Other shape/profile witnesses need separate review.

## Case 7705: tnorm, constant--0.327424-1e-12

Data: `[-0.32742383907552863, -0.3274238390752012, -0.32742383907487377, -0.32742383907454636, -0.3274238390738915]`. Holds: `{'mean': -0.062005588372027076, 'sd': 1.0043282104167566}`.
Call: `route_tnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7705` (run through the slot wrapper).
Returned: `{'mean': '-0.062005588372027076', 'sd': '1.0043282104167566', 'a': '-0.32742383907552863', 'b': '-0.3274238390738915'}`, status `0`, logLik `135.690350558132053038207232020795345306396484375`.
Independent logLik at that point: `135.6903671556373983208673456` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `1.22e-07`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/tnorm/mle.hpp:411`: The held mean/sd likelihood standardizes a and b separately before computing the mass; on a 1e-12-wide window this rounds away part of its width. The raw-endpoint high-precision interval mass gives a different returned-point logLik. This is the suspected arithmetic seam.

## Case 7708: tnorm, constant-1-1e-12

Data: `[0.999999999998, 0.999999999999, 1.0, 1.000000000001, 1.000000000003]`. Holds: `{'mean': -0.062005588372027076, 'sd': 1.0043282104167566}`.
Call: `route_tnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 7708` (run through the slot wrapper).
Returned: `{'mean': '-0.062005588372027076', 'sd': '1.0043282104167566', 'a': '0.99999999999800004', 'b': '1.000000000003'}`, status `0`, logLik `130.1079060989950448856689035892486572265625`.
Independent logLik at that point: `130.1079156037719806186996219` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `7.31e-08`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/tnorm/mle.hpp:411`: The held mean/sd likelihood standardizes a and b separately before computing the mass; on a 1e-12-wide window this rounds away part of its width. The raw-endpoint high-precision interval mass gives a different returned-point logLik. This is the suspected arithmetic seam.

## Case 8173: triang, lower-1e-17

Data: `[0.03627073533558047, 0.036270735335580485, 0.03627073533558049, 0.03627073533558051, 0.036270735335580555]`. Holds: `{}`.
Call: `route_triang(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 8173` (run through the slot wrapper).
Returned: `{'a': '0.036270735335580472', 'b': '0.036270735335580576', 'c': '0.036270735335580472'}`, status `0`, logLik `184.986039586942723644824582152068614959716796875`.
Independent logLik at that point: `184.9859234257181152575022067` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `6.28e-07`.
Better feasible point: `[0.03627073533558047152558501125, 0.03627073533558057595409394083, 0.03627073533558047152558501125]`; logLik `184.9860395869427506389401037`.
Stationary local point: `[0.03627073533558047152558501125, 0.03627073533558057595409394083, 0.03627073533558047152558501125]`; logLik `184.9860395869427506389401037`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp:229`: Affine endpoint restoration can put the returned upper bound one ULP below the data; the final gate checks coefficient ordering but not all original observations. Recheck original-data support before Ok.

## Case 8194: triang, lower-1e-17

Data: `[0.03627073533558047, 0.036270735335580485, 0.03627073533558049, 0.03627073533558051, 0.036270735335580555]`. Holds: `{'a': 0.036270735335580465}`.
Call: `route_triang(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 8194` (run through the slot wrapper).
Returned: `{'a': '0.036270735335580465', 'b': '0.036270735335580576', 'c': '0.036270735335580472'}`, status `0`, logLik `184.66675380319856003552558831870555877685546875`.
Independent logLik at that point: `184.6632308200302593991375871` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `1.91e-05`.
Better feasible point: `[0.03627073533558046458669110734, 0.03627073533558057759192348456, 0.03627073533558047152558501125]`; logLik `184.6667538031985890255928491`.
Stationary local point: `[0.03627073533558046458669110734, 0.03627073533558057759192348456, 0.03627073533558047152558501125]`; logLik `184.6667538031985890255928491`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp:229`: Affine endpoint restoration can put the returned upper bound one ULP below the data; the final gate checks coefficient ordering but not all original observations. Recheck original-data support before Ok.

## Case 8195: triang, scale-1e+150

Data: `[2.1270091277547183e+149, 5.56117281364277e+149, 5.239257282111793e+149, 3.8924929029308256e+149, 5.514445431996137e+149, 5.285874607041478e+149, 5.398522144183517e+149, 3.251642995048314e+149, 6.451318151317536e+149, 4.357297092676162e+149, 5.2674134634435175e+149, 3.8801221367903053e+149]`. Holds: `{'a': 0.036270735335580465}`.
Call: `route_triang(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 8195` (run through the slot wrapper).
Returned: `{'a': '0.036270735335580465', 'b': '6.4513181513175353e+149', 'c': '6.4513181513175353e+149'}`, status `0`, logLik `-4135.370214786225`.
Independent logLik at that point: `-inf` (229 digits).
Evidence: returned coefficients exclude an observation from support. Worst relative discrepancy: `inf`.
Exact support exclusion: `{'bound': 'upper', 'observation_index': 8, 'exact_bound': '645131815131753533751764071048632586031124218536647871008692040239278382221747975012639034839154887813009768508475760158779911528290276866338553069568', 'exact_observation': '645131815131753624606604607999493904696600204537214665213862126154035917408022872592550049013895303586891107728921455254095112311562518558163756646400', 'exact_gap': '90854840536950861318665475986000566794205170085914757535186274897579911014174740415773881339220445695095315200783272241691825203576832', 'bound_expression': 'p_b', 'basis': 'exact binary64 rational support comparison'}`. This is a returned-coefficient feasibility defect; a one-ULP projection can be practically small even though the exact returned-point likelihood is zero.
Suspected cause / path: Boundary reconstruction/projection leaves the exact returned support short of the data; inspect `univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp:229`: Affine endpoint restoration can put the returned upper bound one ULP below the data; the final gate checks coefficient ordering but not all original observations. Recheck original-data support before Ok.

## Case 8268: triang, lower-1e-12

Data: `[0.03627073533658046, 0.03627073533758046, 0.03627073533858047, 0.036270735340580465, 0.03627073534458047]`. Holds: `{'a': 0.036270735335580465, 'c': 0.3888433021027595}`.
Call: `route_triang(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 8268` (run through the slot wrapper).
Returned: `{'a': '0.036270735335580465', 'b': '0.38884330210275952', 'c': '0.38884330210275952'}`, status `0`, logLik `-118.66588676399277346718008629977703094482421875`.
Independent logLik at that point: `-118.6659608528448331118253377` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `6.24e-07`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp:229`: Affine endpoint restoration can put the returned upper bound one ULP below the data; the final gate checks coefficient ordering but not all original observations. Recheck original-data support before Ok.

## Case 8289: triang, upper-1e-12

Data: `[1.0045256123717767, 1.0045256123707769, 1.0045256123697768, 1.0045256123677768, 1.004525612363777]`. Holds: `{'b': 1.0045256123727768, 'c': 0.3888433021027595}`.
Call: `route_triang(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 8289` (run through the slot wrapper).
Returned: `{'a': '0.38884330210275952', 'b': '1.0045256123727768', 'c': '0.38884330210275952'}`, status `0`, logLik `-124.2404976485316865364438854157924652099609375`.
Independent logLik at that point: `-124.2406339751489594770920724` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `1.1e-06`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp:229`: Affine endpoint restoration can put the returned upper bound one ULP below the data; the final gate checks coefficient ordering but not all original observations. Recheck original-data support before Ok.

## Case 8377: weibull, constant-2.89053-1e-12

Data: `[2.8905306440318723, 2.890530644034763, 2.8905306440376535, 2.890530644040544, 2.890530644046325]`. Holds: `{}`.
Call: `route_weibull(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 8377` (run through the slot wrapper).
Returned: `{'shape': '586402656304.12976', 'scale': '2.8905306440408021'}`, status `0`, logLik `122.571518239692039742294582538306713104248046875`.
Independent logLik at that point: `122.5714132060599048128141323` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `8.57e-07`.
Stationary local point: `[586390098658.5239617764449984, 2.890530644040802392595233523]`; logLik `122.5714132227105625773919349`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/weibull/mle.hpp:29`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 8380: weibull, constant-2.89053-1e-12

Data: `[2.8905306440318723, 2.890530644034763, 2.8905306440376535, 2.890530644040544, 2.890530644046325]`. Holds: `{}`.
Call: `route_weibull(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 8380` (run through the slot wrapper).
Returned: `{'shape': '586402656304.12976', 'scale': '2.8905306440408021'}`, status `0`, logLik `122.571518239692039742294582538306713104248046875`.
Independent logLik at that point: `122.5714132060599048128141323` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `8.57e-07`.
Stationary local point: `[586390098658.5239617764449984, 2.890530644040802392595233523]`; logLik `122.5714132227105625773919349`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/weibull/mle.hpp:29`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 8443: weibull, scale-1e+150

Data: `[4.265248796628033e+150, 4.38489213025162e+150, 2.452995601068545e+150, 1.7752922400553187e+150, 2.3617549393577573e+150, 2.068724627912393e+150, 2.8905306440376535e+150, 3.823713579199717e+150, 4.7739544752062214e+150, 1.4304343656494887e+150, 1.2227514133587247e+150, 5.066178480122313e+150]`. Holds: `{'shape': 2.9155677520153174, 'scale': 3.961112786792183}`.
Call: `route_weibull(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 8443` (run through the slot wrapper).
Returned: `{'shape': '2.9155677520153174', 'scale': '3.9611127867921829'}`, status `0`, logLik `-inf`.
Independent logLik at that point: `-1.830030713007254754683608571e+438` (230 digits).
Evidence: status Ok with nonfinite reported logLik. Worst relative discrepancy: `inf`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/weibull/mle.hpp:29`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 8485: zib, large-count

Data: `[1000000, 1000001, 1000003, 1000008, 1000011]`. Holds: `{}`.
Call: `route_zib(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 8485` (run through the slot wrapper).
Returned: `{'size': '1000023', 'prob': '0.99998160042319029', 'pi': '0'}`, status `0`, logLik `-14.3636973961302718549859491758979856967926025390625`.
Independent logLik at that point: `-14.36369671567082418865494152` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `4.74e-08`.
Stationary local point: `[1000023.0, 0.999981600423190266623867651, 0.0]`; logLik `-14.3636967156708241886548872`. Verified coefficient relative error: `—`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/zib/mle.hpp:131` (special difference): `else ll+=std::log1p(-fixed_pi)+v*std::log(lambda)-lambda-special::lgamma(v+1.);`. Execution-path causation remains open.

## Case 8581: zinb, tiny-5

Data: `[2, 6, 0, 2, 2]`. Holds: `{}`.
Call: `route_zinb(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 8581` (run through the slot wrapper).
Returned: `{'size': '11.940058996529116', 'prob': '0.81297478016504199', 'pi': '0.12626110623230022'}`, status `0`, logLik `-9.716934986133782814476944622583687305450439453125`.
Independent logLik at that point: `-9.716934986133782718709569003` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.04e-08`.
Better feasible point: `[11.94005863304279737741186515, 0.8129747756648252124835365518, 0.1262611054939805748071158967]`; logLik `-9.716934986133782694982474045`.
Stationary local point: `[11.94005863304279737741186515, 0.8129747756648252124835365518, 0.1262611054939805748071158967]`; logLik `-9.716934986133782694982474045`. Verified coefficient relative error: `3.04e-08`.
Local information eigenvalue range: `[0.009524895272011497231588368994, 50.82293671515262899958699524]`; per-observation stationarity residual `1.112419079803543949478676193e-54`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/zinb/mle.hpp:86` (special difference): `double gamma_difference = special::lgamma(r + v) - special::lgamma(r);`. Execution-path causation remains open.

## Case 8634: zinb, interior

Data: `[2, 6, 0, 2, 2, 0, 5, 2, 0, 0, 0, 0]`. Holds: `{'pi': 0.2573876163521669}`.
Call: `route_zinb(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 8634` (run through the slot wrapper).
Returned: `{'size': '2.4215604903930159', 'prob': '0.50998729870269621', 'pi': '0.25738761635216689'}`, status `0`, logLik `-19.95298512288456294072602759115397930145263671875`.
Independent logLik at that point: `-19.95298512288456386317587946` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.41e-08`.
Better feasible point: `[2.421560621296339745088109863, 0.5099873148966644051452770896, 0.2573876163521668924971663728]`; logLik `-19.95298512288456236094501174`.
Stationary local point: `[2.421560621296339745088109863, 0.5099873148966644051452770896, 0.2573876163521668924971663728]`; logLik `-19.95298512288456236094501174`. Verified coefficient relative error: `5.41e-08`.
Local information eigenvalue range: `[0.0252861057933978726348229282, 3.220873637100399123937163718]`; per-observation stationarity residual `9.111820425773008443781758947e-57`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/zinb/mle.hpp:86` (special difference): `double gamma_difference = special::lgamma(r + v) - special::lgamma(r);`. Execution-path causation remains open.

## Case 8805: zmbinom, large-count

Data: `[1000000, 1000001, 1000003, 1000008, 1000011]`. Holds: `{}`.
Call: `route_zmbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 8805` (run through the slot wrapper).
Returned: `{'size': '1000023', 'prob': '0.99998160042319029', 'p0': '0'}`, status `0`, logLik `-14.3636973961302718549859491758979856967926025390625`.
Independent logLik at that point: `-14.36369671567082418865494152` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `4.74e-08`.
Stationary local point: `[1000023.0, 0.999981600423190266623867651, 0.0]`; logLik `-14.3636967156708241886548872`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 8857: zmbinom, large-count

Data: `[1000000, 1000001, 1000003, 1000008, 1000011]`. Holds: `{'p0': 0.276}`.
Call: `route_zmbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 8857` (run through the slot wrapper).
Returned: `{'size': '1000023', 'prob': '0.99998160042319029', 'p0': '0.27600000000000002'}`, status `0`, logLik `-15.978516829112376029797815135680139064788818359375`.
Independent logLik at that point: `-15.97851614865292778003660743` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `4.26e-08`.
Stationary local point: `[1000023.0, 0.999981600423190266623867651, 0.2760000000000000230926389122]`; logLik `-15.97851614865292778003655311`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9010: zmnbinom, interior

Data: `[6, 0, 1, 6, 4, 1, 7, 0, 4, 1, 3, 10]`. Holds: `{}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9010` (run through the slot wrapper).
Returned: `{'size': '2.0489931241318518', 'prob': '0.35040759058301307', 'p0': '0.16666666666666666'}`, status `0`, logLik `-28.563825370654907231937613687478005886077880859375`.
Independent logLik at that point: `-28.56382537065491560023770545` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.19e-07`.
Better feasible point: `[2.048992880297860960745445171, 0.3504075670435691181632916711, 0.1666666666666666666666666667]`; logLik `-28.56382537065491053137363976`.
Stationary local point: `[2.048992880297860960745445171, 0.3504075670435691181632916711, 0.1666666666666666666666666667]`; logLik `-28.56382537065491053137363976`. Verified coefficient relative error: `1.19e-07`.
Local information eigenvalue range: `[0.04503580127500615925188852102, 3.2488967999202251538170429]`; per-observation stationarity residual `4.793372915867428616103898498e-55`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9012: zmnbinom, tiny-3

Data: `[6, 0, 1]`. Holds: `{}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9012` (run through the slot wrapper).
Returned: `{'size': '0.4000087706216191', 'prob': '0.19089835381284642', 'p0': '0.33333333333333331'}`, status `0`, logLik `-6.07555412046320952867972664535045623779296875`.
Independent logLik at that point: `-6.075554120463211058642481384` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.43e-08`.
Better feasible point: `[0.4000087609102296194826731, 0.1908983521257934310280881001, 0.3333333333333333333333333333]`; logLik `-6.075554120463211049427531453`.
Stationary local point: `[0.4000087609102296194826731, 0.1908983521257934310280881001, 0.3333333333333333333333333333]`; logLik `-6.075554120463211049427531453`. Verified coefficient relative error: `2.43e-08`.
Local information eigenvalue range: `[0.009180611929011262976738169933, 0.5063539520913260093540188296]`; per-observation stationarity residual `4.524041077508992191057888473e-65`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9013: zmnbinom, tiny-5

Data: `[6, 0, 1, 6, 4]`. Holds: `{}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9013` (run through the slot wrapper).
Returned: `{'size': '37.784087323894759', 'prob': '0.90063762223904043', 'p0': '0.20000000000000001'}`, status `0`, logLik `-11.1763595032815050700492065516300499439239501953125`.
Independent logLik at that point: `-11.17635950328151143210818329` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.52e-07`.
Better feasible point: `[37.78406269302506316165394081, 0.9006375641481675919546139262, 0.2]`; logLik `-11.17635950328150949609442714`.
Stationary local point: `[37.78406269302506316165394081, 0.9006375641481675919546139262, 0.2]`; logLik `-11.17635950328150949609442714`. Verified coefficient relative error: `6.52e-07`.
Local information eigenvalue range: `[0.001804663875374766170581022708, 290.2091448644231082506047257]`; per-observation stationarity residual `8.1976924417051912629537242e-69`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9038: zmnbinom, interior

Data: `[6, 0, 1, 6, 4, 1, 7, 0, 4, 1, 3, 10]`. Holds: `{'prob': 0.41499877689089204}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9038` (run through the slot wrapper).
Returned: `{'size': '2.7403388023376465', 'prob': '0.41499877689089204', 'p0': '0.16666666666666666'}`, status `0`, logLik `-28.596121035100257046224214718677103519439697265625`.
Independent logLik at that point: `-28.5961210351002511720012806` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.7e-07`.
Better feasible point: `[2.740339269319108696961637116, 0.4149987768908920404697937556, 0.1666666666666666666666666667]`; logLik `-28.59612103510006640477516252`.
Stationary local point: `[2.740339269319108696961637116, 0.4149987768908920404697937556, 0.1666666666666666666666666667]`; logLik `-28.59612103510006640477516252`. Verified coefficient relative error: `1.7e-07`.
Local information eigenvalue range: `[0.1999999999999999777955395075, 1.06043219210503667284228659]`; per-observation stationarity residual `5.097237784078428804169080215e-57`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9039: zmnbinom, tiny-2

Data: `[6, 0]`. Holds: `{'prob': 0.41499877689089204}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9039` (run through the slot wrapper).
Returned: `{'size': '4.6337462207161462', 'prob': '0.41499877689089204', 'p0': '0.5'}`, status `0`, logLik `-3.632706733405950405568773931008763611316680908203125`.
Independent logLik at that point: `-3.632706733405951305906514299` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.43e-08`.
Better feasible point: `[4.633746154429836693693340887, 0.4149987768908920404697937556, 0.5]`; logLik `-3.632706733405951023475237499`.
Stationary local point: `[4.633746154429836693693340887, 0.4149987768908920404697937556, 0.5]`; logLik `-3.632706733405951023475237499`. Verified coefficient relative error: `1.43e-08`.
Local information eigenvalue range: `[1.0, 1.380160524270965589930572241]`; per-observation stationarity residual `1.084398010024140667966354293e-64`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9040: zmnbinom, tiny-3

Data: `[6, 0, 1]`. Holds: `{'prob': 0.41499877689089204}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9040` (run through the slot wrapper).
Returned: `{'size': '1.915516274145721', 'prob': '0.41499877689089204', 'p0': '0.33333333333333331'}`, status `0`, logLik `-6.16200567340111859948592609725892543792724609375`.
Independent logLik at that point: `-6.162005673401120170487814989` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.9e-08`.
Better feasible point: `[1.915516218665101145162866202, 0.4149987768908920404697937556, 0.3333333333333333333333333333]`; logLik `-6.162005673401119576049716376`.
Stationary local point: `[1.915516218665101145162866202, 0.4149987768908920404697937556, 0.3333333333333333333333333333]`; logLik `-6.162005673401119576049716376`. Verified coefficient relative error: `2.9e-08`.
Local information eigenvalue range: `[0.4723940986257405460813253034, 0.4999999999999999444888487687]`; per-observation stationarity residual `3.938617979144151921727020681e-64`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9041: zmnbinom, tiny-5

Data: `[6, 0, 1, 6, 4]`. Holds: `{'prob': 0.41499877689089204}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9041` (run through the slot wrapper).
Returned: `{'size': '2.8949165310919214', 'prob': '0.41499877689089204', 'p0': '0.20000000000000001'}`, status `0`, logLik `-11.398795230970119263247397611849009990692138671875`.
Independent logLik at that point: `-11.39879523097012182412418205` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.78e-08`.
Better feasible point: `[2.894916450737668125149784162, 0.4149987768908920404697937556, 0.2]`; logLik `-11.39879523097011964435274319`.
Stationary local point: `[2.894916450737668125149784162, 0.4149987768908920404697937556, 0.2]`; logLik `-11.39879523097011964435274319`. Verified coefficient relative error: `2.78e-08`.
Local information eigenvalue range: `[0.2500000000000000277555756156, 1.131684341588574477205984324]`; per-observation stationarity residual `3.610890580983594326488621834e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9051: zmnbinom, constant-count

Data: `[100, 100, 100, 101, 100]`. Holds: `{'prob': 0.41499877689089204}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9051` (run through the slot wrapper).
Returned: `{'size': '71.580530097338666', 'prob': '0.41499877689089204', 'p0': '0'}`, status `0`, logLik `-18.31538786169813448623244767077267169952392578125`.
Independent logLik at that point: `-18.31538786169898611713214845` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.3e-08`.
Better feasible point: `[71.58053317445742968221364693, 0.4149987768908920404697937556, 0.0]`; logLik `-18.31538786169879130117353122`.
Stationary local point: `[71.58053317445742968221364693, 0.4149987768908920404697937556, 0.0]`; logLik `-18.31538786169879130117353122`. Verified coefficient relative error: `4.3e-08`.
Local information eigenvalue range: `[42.16827311028030317674207965, 42.16827311028030317674207965]`; per-observation stationarity residual `4.582145703820048104002728978e-59`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9066: zmnbinom, interior

Data: `[6, 0, 1, 6, 4, 1, 7, 0, 4, 1, 3, 10]`. Holds: `{'p0': 0.314}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9066` (run through the slot wrapper).
Returned: `{'size': '2.0489931241318518', 'prob': '0.35040759058301307', 'p0': '0.314'}`, status `0`, logLik `-29.242591962969537888739068876020610332489013671875`.
Independent logLik at that point: `-29.24259196296954499106203446` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.19e-07`.
Better feasible point: `[2.048992880297860960745445171, 0.3504075670435691181632916711, 0.3140000000000000013322676296]`; logLik `-29.24259196296953992219796877`.
Stationary local point: `[2.048992880297860960745445171, 0.3504075670435691181632916711, 0.3140000000000000013322676296]`; logLik `-29.24259196296953992219796877`. Verified coefficient relative error: `1.19e-07`.
Local information eigenvalue range: `[0.04503580127500615925188852102, 3.2488967999202251538170429]`; per-observation stationarity residual `4.793372915867428616103898498e-55`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9068: zmnbinom, tiny-3

Data: `[6, 0, 1]`. Holds: `{'p0': 0.314}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9068` (run through the slot wrapper).
Returned: `{'size': '0.4000087706216191', 'prob': '0.19089835381284642', 'p0': '0.314'}`, status `0`, logLik `-6.07812921116515791908341270755045115947723388671875`.
Independent logLik at that point: `-6.078129211165159942786146592` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.43e-08`.
Better feasible point: `[0.4000087609102296194826731, 0.1908983521257934310280881001, 0.3140000000000000013322676296]`; logLik `-6.078129211165159933571196661`.
Stationary local point: `[0.4000087609102296194826731, 0.1908983521257934310280881001, 0.3140000000000000013322676296]`; logLik `-6.078129211165159933571196661`. Verified coefficient relative error: `2.43e-08`.
Local information eigenvalue range: `[0.009180611929011262976738169933, 0.5063539520913260093540188296]`; per-observation stationarity residual `4.524041077508992191057888473e-65`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9069: zmnbinom, tiny-5

Data: `[6, 0, 1, 6, 4]`. Holds: `{'p0': 0.314}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9069` (run through the slot wrapper).
Returned: `{'size': '37.784087323894759', 'prob': '0.90063762223904043', 'p0': '0.314'}`, status `0`, logLik `-11.340220283689458113940418115817010402679443359375`.
Independent logLik at that point: `-11.34022028368946303246271048` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.52e-07`.
Better feasible point: `[37.78406269302506316165394081, 0.9006375641481675919546139262, 0.3140000000000000013322676296]`; logLik `-11.34022028368946109644895433`.
Stationary local point: `[37.78406269302506316165394081, 0.9006375641481675919546139262, 0.3140000000000000013322676296]`; logLik `-11.34022028368946109644895433`. Verified coefficient relative error: `6.52e-07`.
Local information eigenvalue range: `[0.001804663875374766170581022708, 290.2091448644231082506047257]`; per-observation stationarity residual `8.1976924417051912629537242e-69`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9094: zmnbinom, interior

Data: `[6, 0, 1, 6, 4, 1, 7, 0, 4, 1, 3, 10]`. Holds: `{'prob': 0.41499877689089204, 'p0': 0.314}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9094` (run through the slot wrapper).
Returned: `{'size': '2.7403388023376465', 'prob': '0.41499877689089204', 'p0': '0.314'}`, status `0`, logLik `-29.274887627414887703025669907219707965850830078125`.
Independent logLik at that point: `-29.27488762741488056282560961` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.7e-07`.
Better feasible point: `[2.740339269319108696961637116, 0.4149987768908920404697937556, 0.3140000000000000013322676296]`; logLik `-29.27488762741469579559949153`.
Stationary local point: `[2.740339269319108696961637116, 0.4149987768908920404697937556, 0.3140000000000000013322676296]`; logLik `-29.27488762741469579559949153`. Verified coefficient relative error: `1.7e-07`.
Local information eigenvalue range: `[1.06043219210503667284228659, 1.06043219210503667284228659]`; per-observation stationarity residual `5.09723778407842880416906897e-57`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9095: zmnbinom, tiny-2

Data: `[6, 0]`. Holds: `{'prob': 0.41499877689089204, 'p0': 0.314}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9095` (run through the slot wrapper).
Returned: `{'size': '4.6337462207161462', 'prob': '0.41499877689089204', 'p0': '0.314'}`, status `0`, logLik `-3.781652316616195275145173582131974399089813232421875`.
Independent logLik at that point: `-3.781652316616196197304221627` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.43e-08`.
Better feasible point: `[4.633746154429836693693340887, 0.4149987768908920404697937556, 0.3140000000000000013322676296]`; logLik `-3.781652316616195914872944827`.
Stationary local point: `[4.633746154429836693693340887, 0.4149987768908920404697937556, 0.3140000000000000013322676296]`; logLik `-3.781652316616195914872944827`. Verified coefficient relative error: `1.43e-08`.
Local information eigenvalue range: `[1.380160524270965589930572241, 1.380160524270965589930572241]`; per-observation stationarity residual `1.084398010024140667966354293e-64`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9096: zmnbinom, tiny-3

Data: `[6, 0, 1]`. Holds: `{'prob': 0.41499877689089204, 'p0': 0.314}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9096` (run through the slot wrapper).
Returned: `{'size': '1.915516274145721', 'prob': '0.41499877689089204', 'p0': '0.314'}`, status `0`, logLik `-6.164580764103067878068031859584152698516845703125`.
Independent logLik at that point: `-6.164580764103069054631480197` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.9e-08`.
Better feasible point: `[1.915516218665101145162866202, 0.4149987768908920404697937556, 0.3140000000000000013322676296]`; logLik `-6.164580764103068460193381584`.
Stationary local point: `[1.915516218665101145162866202, 0.4149987768908920404697937556, 0.3140000000000000013322676296]`; logLik `-6.164580764103068460193381584`. Verified coefficient relative error: `2.9e-08`.
Local information eigenvalue range: `[0.4723940986257405460813253034, 0.4723940986257405460813253034]`; per-observation stationarity residual `3.938617979144151921727020681e-64`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9097: zmnbinom, tiny-5

Data: `[6, 0, 1, 6, 4]`. Holds: `{'prob': 0.41499877689089204, 'p0': 0.314}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9097` (run through the slot wrapper).
Returned: `{'size': '2.8949165310919214', 'prob': '0.41499877689089204', 'p0': '0.314'}`, status `0`, logLik `-11.5626560113780723071386091760359704494476318359375`.
Independent logLik at that point: `-11.56265601137807342447870924` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.78e-08`.
Better feasible point: `[2.894916450737668125149784162, 0.4149987768908920404697937556, 0.3140000000000000013322676296]`; logLik `-11.56265601137807124470727038`.
Stationary local point: `[2.894916450737668125149784162, 0.4149987768908920404697937556, 0.3140000000000000013322676296]`; logLik `-11.56265601137807124470727038`. Verified coefficient relative error: `2.78e-08`.
Local information eigenvalue range: `[1.131684341588574477205984324, 1.131684341588574477205984324]`; per-observation stationarity residual `3.610890580983594326488621834e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9107: zmnbinom, constant-count

Data: `[100, 100, 100, 101, 100]`. Holds: `{'prob': 0.41499877689089204, 'p0': 0.314}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9107` (run through the slot wrapper).
Returned: `{'size': '71.580530097338666', 'prob': '0.41499877689089204', 'p0': '0.314'}`, status `0`, logLik `-20.199776117979393319501468795351684093475341796875`.
Independent logLik at that point: `-20.19977611798024526344597397` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.3e-08`.
Better feasible point: `[71.58053317445742968221364693, 0.4149987768908920404697937556, 0.3140000000000000013322676296]`; logLik `-20.19977611798005044748735675`.
Stationary local point: `[71.58053317445742968221364693, 0.4149987768908920404697937556, 0.3140000000000000013322676296]`; logLik `-20.19977611798005044748735675`. Verified coefficient relative error: `4.3e-08`.
Local information eigenvalue range: `[42.16827311028030317674207965, 42.16827311028030317674207965]`; per-observation stationarity residual `4.582145703820048104002728978e-59`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9189: ztbinom, large-count

Data: `[1000000, 1000001, 1000003, 1000008, 1000011]`. Holds: `{}`.
Call: `route_ztbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9189` (run through the slot wrapper).
Returned: `{'size': '1000023', 'prob': '0.99998160042319029'}`, status `0`, logLik `-14.3636973961302718549859491758979856967926025390625`.
Independent logLik at that point: `-14.36369671567082418865494152` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `4.74e-08`.
Stationary local point: `[1000023.0, 0.999981600423190266623867651]`; logLik `-14.3636967156708241886548872`. Verified coefficient relative error: `—`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ztbinom/mle.hpp:15` (step stop): `for(int i=0;i<maxit && hi-lo>tol;++i) {`. Execution-path causation remains open.

## Case 9240: ztnbinom, interior

Data: `[5, 4, 1, 5, 1, 2, 2, 1, 9, 3, 3, 2]`. Holds: `{}`.
Call: `route_ztnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9240` (run through the slot wrapper).
Returned: `{'size': '2.275699858392529', 'prob': '0.46577190809304125'}`, status `0`, logLik `-23.505843470835174002786516211926937103271484375`.
Independent logLik at that point: `-23.5058434708351830384854564` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.22e-08`.
Better feasible point: `[2.275699739540539697527133869, 0.4657718970537771661592979986]`; logLik `-23.50584347083518219383538337`.
Stationary local point: `[2.275699739540539697527133869, 0.4657718970537771661592979986]`; logLik `-23.50584347083518219383538337`. Verified coefficient relative error: `5.22e-08`.
Local information eigenvalue range: `[0.04271488213846258475034749022, 4.625787464182972154331224167]`; per-observation stationarity residual `3.320644871879916397290537333e-57`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ztnbinom/mle.hpp:34` (step stop): `if(maxit < std::ceil(std::log(std::min(tol,0.5))/std::log(0.6180339887498948482)))`. Execution-path causation remains open.

## Case 9243: ztnbinom, tiny-5

Data: `[5, 4, 1, 5, 1]`. Holds: `{}`.
Call: `route_ztnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9243` (run through the slot wrapper).
Returned: `{'size': '4.2379571178868094', 'prob': '0.59919094988316268'}`, status `0`, logLik `-9.7927323567418174121712581836618483066558837890625`.
Independent logLik at that point: `-9.792732356741823527618466396` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.21e-08`.
Better feasible point: `[4.237957211444497181520098334, 0.599190954772396218042520972]`; logLik `-9.792732356741823500330528655`.
Stationary local point: `[4.237957211444497181520098334, 0.599190954772396218042520972]`; logLik `-9.792732356741823500330528655`. Verified coefficient relative error: `2.21e-08`.
Local information eigenvalue range: `[0.01969972391533738869176194176, 10.60052636967110607455356964]`; per-observation stationarity residual `5.122987049213641301769990595e-58`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ztnbinom/mle.hpp:34` (step stop): `if(maxit < std::ceil(std::log(std::min(tol,0.5))/std::log(0.6180339887498948482)))`. Execution-path causation remains open.

## Case 9266: ztnbinom, interior

Data: `[5, 4, 1, 5, 1, 2, 2, 1, 9, 3, 3, 2]`. Holds: `{'prob': 0.4269728320855304}`.
Call: `route_ztnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9266` (run through the slot wrapper).
Returned: `{'size': '1.910684515585009', 'prob': '0.42697283208553038'}`, status `0`, logLik `-23.516011099399889872074709273874759674072265625`.
Independent logLik at that point: `-23.5160110993998984150682039` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.95e-08`.
Better feasible point: `[1.910684459217950491803919602, 0.4269728320855303849157280638]`; logLik `-23.51601109939989459637434589`.
Stationary local point: `[1.910684459217950491803919602, 0.4269728320855303849157280638]`; logLik `-23.51601109939989459637434589`. Verified coefficient relative error: `2.95e-08`.
Local information eigenvalue range: `[0.7312919975173448057944820425, 0.7312919975173448057944820425]`; per-observation stationarity residual `1.489232443976104281942737968e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ztnbinom/mle.hpp:34` (step stop): `if(maxit < std::ceil(std::log(std::min(tol,0.5))/std::log(0.6180339887498948482)))`. Execution-path causation remains open.

## Case 9268: ztnbinom, tiny-3

Data: `[5, 4, 1]`. Holds: `{'prob': 0.4269728320855304}`.
Call: `route_ztnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9268` (run through the slot wrapper).
Returned: `{'size': '2.1887923725399361', 'prob': '0.42697283208553038'}`, status `0`, logLik `-5.9659492070845434597003986709751188755035400390625`.
Independent logLik at that point: `-5.965949207084545241677868243` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.95e-08`.
Better feasible point: `[2.188792329873531539973478452, 0.4269728320855303849157280638]`; logLik `-5.965949207084544727797416665`.
Stationary local point: `[2.188792329873531539973478452, 0.4269728320855303849157280638]`; logLik `-5.965949207084544727797416665`. Verified coefficient relative error: `1.95e-08`.
Local information eigenvalue range: `[0.9015874967124586009608222857, 0.9015874967124586009608222857]`; per-observation stationarity residual `8.095794353906148226796097675e-65`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ztnbinom/mle.hpp:34` (step stop): `if(maxit < std::ceil(std::log(std::min(tol,0.5))/std::log(0.6180339887498948482)))`. Execution-path causation remains open.

## Case 9270: ztnbinom, ties

Data: `[5, 5, 5, 5, 4, 1]`. Holds: `{'prob': 0.4269728320855304}`.
Call: `route_ztnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9270` (run through the slot wrapper).
Returned: `{'size': '3.1197973685652274', 'prob': '0.42697283208553038'}`, status `0`, logLik `-12.761221679518740756975603289902210235595703125`.
Independent logLik at that point: `-12.76122167951874991470449248` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.24e-08`.
Better feasible point: `[3.11979740711266955613283095, 0.4269728320855303849157280638]`; logLik `-12.76122167951874920088010239`.
Stationary local point: `[3.11979740711266955613283095, 0.4269728320855303849157280638]`; logLik `-12.76122167951874920088010239`. Verified coefficient relative error: `1.24e-08`.
Local information eigenvalue range: `[1.558589585132821628472206251, 1.558589585132821628472206251]`; per-observation stationarity residual `1.125433592981988013979390988e-65`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ztnbinom/mle.hpp:34` (step stop): `if(maxit < std::ceil(std::log(std::min(tol,0.5))/std::log(0.6180339887498948482)))`. Execution-path causation remains open.

## Case 9271: ztnbinom, constant-3-0.0001

Data: `[2, 2, 3, 3, 3]`. Holds: `{'prob': 0.4269728320855304}`.
Call: `route_ztnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9271` (run through the slot wrapper).
Returned: `{'size': '1.7335182019140856', 'prob': '0.42697283208553038'}`, status `0`, logLik `-8.3474397684713625977792617049999535083770751953125`.
Independent logLik at that point: `-8.347439768471365847735272734` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.35e-08`.
Better feasible point: `[1.73351814381096859155960204, 0.4269728320855303849157280638]`; logLik `-8.347439768471363896516819491`.
Stationary local point: `[1.73351814381096859155960204, 0.4269728320855303849157280638]`; logLik `-8.347439768471363896516819491`. Verified coefficient relative error: `3.35e-08`.
Local information eigenvalue range: `[0.6947424578668869216324498522, 0.6947424578668869216324498522]`; per-observation stationarity residual `6.269986785020528058357776115e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ztnbinom/mle.hpp:34` (step stop): `if(maxit < std::ceil(std::log(std::min(tol,0.5))/std::log(0.6180339887498948482)))`. Execution-path causation remains open.

## Case 9272: ztnbinom, constant-3-1e-08

Data: `[2, 2, 3, 3, 3]`. Holds: `{'prob': 0.4269728320855304}`.
Call: `route_ztnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9272` (run through the slot wrapper).
Returned: `{'size': '1.7335182019140856', 'prob': '0.42697283208553038'}`, status `0`, logLik `-8.3474397684713625977792617049999535083770751953125`.
Independent logLik at that point: `-8.347439768471365847735272734` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.35e-08`.
Better feasible point: `[1.73351814381096859155960204, 0.4269728320855303849157280638]`; logLik `-8.347439768471363896516819491`.
Stationary local point: `[1.73351814381096859155960204, 0.4269728320855303849157280638]`; logLik `-8.347439768471363896516819491`. Verified coefficient relative error: `3.35e-08`.
Local information eigenvalue range: `[0.6947424578668869216324498522, 0.6947424578668869216324498522]`; per-observation stationarity residual `6.269986785020528058357776115e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ztnbinom/mle.hpp:34` (step stop): `if(maxit < std::ceil(std::log(std::min(tol,0.5))/std::log(0.6180339887498948482)))`. Execution-path causation remains open.

## Case 9273: ztnbinom, constant-3-1e-12

Data: `[2, 2, 3, 3, 3]`. Holds: `{'prob': 0.4269728320855304}`.
Call: `route_ztnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9273` (run through the slot wrapper).
Returned: `{'size': '1.7335182019140856', 'prob': '0.42697283208553038'}`, status `0`, logLik `-8.3474397684713625977792617049999535083770751953125`.
Independent logLik at that point: `-8.347439768471365847735272734` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.35e-08`.
Better feasible point: `[1.73351814381096859155960204, 0.4269728320855303849157280638]`; logLik `-8.347439768471363896516819491`.
Stationary local point: `[1.73351814381096859155960204, 0.4269728320855303849157280638]`; logLik `-8.347439768471363896516819491`. Verified coefficient relative error: `3.35e-08`.
Local information eigenvalue range: `[0.6947424578668869216324498522, 0.6947424578668869216324498522]`; per-observation stationarity residual `6.269986785020528058357776115e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ztnbinom/mle.hpp:34` (step stop): `if(maxit < std::ceil(std::log(std::min(tol,0.5))/std::log(0.6180339887498948482)))`. Execution-path causation remains open.

## Case 9274: ztnbinom, constant-3-0.0001

Data: `[2, 2, 3, 3, 3]`. Holds: `{'prob': 0.4269728320855304}`.
Call: `route_ztnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9274` (run through the slot wrapper).
Returned: `{'size': '1.7335182019140856', 'prob': '0.42697283208553038'}`, status `0`, logLik `-8.3474397684713625977792617049999535083770751953125`.
Independent logLik at that point: `-8.347439768471365847735272734` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.35e-08`.
Better feasible point: `[1.73351814381096859155960204, 0.4269728320855303849157280638]`; logLik `-8.347439768471363896516819491`.
Stationary local point: `[1.73351814381096859155960204, 0.4269728320855303849157280638]`; logLik `-8.347439768471363896516819491`. Verified coefficient relative error: `3.35e-08`.
Local information eigenvalue range: `[0.6947424578668869216324498522, 0.6947424578668869216324498522]`; per-observation stationarity residual `6.269986785020528058357776115e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ztnbinom/mle.hpp:34` (step stop): `if(maxit < std::ceil(std::log(std::min(tol,0.5))/std::log(0.6180339887498948482)))`. Execution-path causation remains open.

## Case 9275: ztnbinom, constant-3-1e-08

Data: `[2, 2, 3, 3, 3]`. Holds: `{'prob': 0.4269728320855304}`.
Call: `route_ztnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9275` (run through the slot wrapper).
Returned: `{'size': '1.7335182019140856', 'prob': '0.42697283208553038'}`, status `0`, logLik `-8.3474397684713625977792617049999535083770751953125`.
Independent logLik at that point: `-8.347439768471365847735272734` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.35e-08`.
Better feasible point: `[1.73351814381096859155960204, 0.4269728320855303849157280638]`; logLik `-8.347439768471363896516819491`.
Stationary local point: `[1.73351814381096859155960204, 0.4269728320855303849157280638]`; logLik `-8.347439768471363896516819491`. Verified coefficient relative error: `3.35e-08`.
Local information eigenvalue range: `[0.6947424578668869216324498522, 0.6947424578668869216324498522]`; per-observation stationarity residual `6.269986785020528058357776115e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ztnbinom/mle.hpp:34` (step stop): `if(maxit < std::ceil(std::log(std::min(tol,0.5))/std::log(0.6180339887498948482)))`. Execution-path causation remains open.

## Case 9276: ztnbinom, constant-3-1e-12

Data: `[2, 2, 3, 3, 3]`. Holds: `{'prob': 0.4269728320855304}`.
Call: `route_ztnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9276` (run through the slot wrapper).
Returned: `{'size': '1.7335182019140856', 'prob': '0.42697283208553038'}`, status `0`, logLik `-8.3474397684713625977792617049999535083770751953125`.
Independent logLik at that point: `-8.347439768471365847735272734` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.35e-08`.
Better feasible point: `[1.73351814381096859155960204, 0.4269728320855303849157280638]`; logLik `-8.347439768471363896516819491`.
Stationary local point: `[1.73351814381096859155960204, 0.4269728320855303849157280638]`; logLik `-8.347439768471363896516819491`. Verified coefficient relative error: `3.35e-08`.
Local information eigenvalue range: `[0.6947424578668869216324498522, 0.6947424578668869216324498522]`; per-observation stationarity residual `6.269986785020528058357776115e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ztnbinom/mle.hpp:34` (step stop): `if(maxit < std::ceil(std::log(std::min(tol,0.5))/std::log(0.6180339887498948482)))`. Execution-path causation remains open.

## Case 9278: ztnbinom, constant-count

Data: `[100, 100, 100, 101, 100]`. Holds: `{'prob': 0.4269728320855304}`.
Call: `route_ztnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9278` (run through the slot wrapper).
Returned: `{'size': '75.159678183654862', 'prob': '0.42697283208553038'}`, status `0`, logLik `-18.244439124144339103850143146701157093048095703125`.
Independent logLik at that point: `-18.24443912414572200617157416` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.04e-08`.
Better feasible point: `[75.15968271980910935580776716, 0.4269728320855303849157280638]`; logLik `-18.24443912414532718956727468`.
Stationary local point: `[75.15968271980910935580776716, 0.4269728320855303849157280638]`; logLik `-18.24443912414532718956727468`. Verified coefficient relative error: `6.04e-08`.
Local information eigenvalue range: `[43.35603753907977398361905461, 43.35603753907977398361905461]`; per-observation stationarity residual `7.526089968710657004647221473e-58`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ztnbinom/mle.hpp:34` (step stop): `if(maxit < std::ceil(std::log(std::min(tol,0.5))/std::log(0.6180339887498948482)))`. Execution-path causation remains open.

## Case 9334: weibull, face-point-mass-100

Data: `[0.968924587752224, 0.9800671524270115, 0.9855643925464165, 0.9894097425847452, 0.9924782821498578, 0.995119748028958, 0.9975204980452366, 0.9998064498474432, 1.0020897352869615, 1.004512085436279, 1.0073478576721242, 1.0116297963585095]`. Holds: `{}`.
Call: `route_weibull(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9334` (run through the slot wrapper).
Returned: `{'shape': '105.80606806484292', 'scale': '0.99993154822791619'}`, status `0`, logLik `37.0528791707155136236906400881707668304443359375`.
Independent logLik at that point: `37.05287917071551522908789219` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.27e-08`.
Better feasible point: `[105.8060704634259402902174421, 0.9999315483180235124641772974]`; logLik `37.05287917071552015894048391`.
Stationary local point: `[105.8060704634259402902174421, 0.9999315483180235124641772974]`; logLik `37.05287917071552015894048391`. Verified coefficient relative error: `2.27e-08`.
Local information eigenvalue range: `[1.598775392752368002870885253, 11195.10146391896713152982017]`; per-observation stationarity residual `1.061930953678997233239678851e-60`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/weibull/mle.hpp:29`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9338: invweibull, face-point-mass-100

Data: `[0.988503900932562, 0.9927057395157377, 0.9955081820301651, 0.9979146225997783, 1.0001935876214705, 1.0024856651664025, 1.004904185632642, 1.0075787228651987, 1.0107036114154169, 1.0146470464667317, 1.0203382467452637, 1.0320720648857378]`. Holds: `{}`.
Call: `route_invweibull(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9338` (run through the slot wrapper).
Returned: `{'shape': '105.80606806484292', 'scale': '1.0000684564580498'}`, status `0`, logLik `36.9198080661555394499373505823314189910888671875`.
Independent logLik at that point: `36.91980806615553812470939237` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.27e-08`.
Better feasible point: `[105.806070463425918743477231, 1.000068456367930012945809393]`; logLik `36.91980806615554305456189551`.
Stationary local point: `[105.806070463425918743477231, 1.000068456367930012945809393]`; logLik `36.91980806615554305456189551`. Verified coefficient relative error: `2.27e-08`.
Local information eigenvalue range: `[1.598775392752378495389827083, 11195.10146795421944257943792]`; per-observation stationarity residual `1.094484039099960128311273394e-60`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/invweibull/mle.hpp:29`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9406: betapr, face-gamma-100

Data: `[0.3197069728799997, 0.6082004232863027, 0.8467494700924845, 1.0750260293010268, 1.3071481463761678, 1.5529388189767, 1.8226027864153431, 2.129903405277392, 2.497238240779209, 2.968377068514949, 3.6543644648852083, 5.063233790738319]`. Holds: `{'shape2': 3.2600098684669976}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9406` (run through the slot wrapper).
Returned: `{'shape1': '3.8586386808927027', 'shape2': '3.2600098684669976', 'scale': '1.2977179909635657'}`, status `0`, logLik `-19.030746300387693992206550319679081439971923828125`.
Independent logLik at that point: `-19.03074630038771035177395722` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.98e-08`.
Better feasible point: `[3.85863861839876454323409817, 3.260009868466997584590671977, 1.297718016606548165224343938]`; logLik `-19.03074630038771008804442015`.
Stationary local point: `[3.85863861839876454323409817, 3.260009868466997584590671977, 1.297718016606548165224343938]`; logLik `-19.03074630038771008804442015`. Verified coefficient relative error: `1.98e-08`.
Local information eigenvalue range: `[0.06648635199272209596105348861, 3.650383620432790134586643964]`; per-observation stationarity residual `1.703969748148940400271588873e-59`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 9413: betapr, face-inverse-gamma-100

Data: `[0.19750223697535038, 0.27364539295655965, 0.33688442435660365, 0.40044237016327694, 0.4695048599491591, 0.5486659010144386, 0.6439403714944437, 0.7650242268042201, 0.9302100346818517, 1.1809868624904873, 1.644194843858683, 3.127864215759038]`. Holds: `{'shape1': 2.014271442794272}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9413` (run through the slot wrapper).
Returned: `{'shape1': '2.0142714427942718', 'shape2': '6.3188795418943871', 'scale': '2.3136327046636751'}`, status `0`, logLik `-8.9872260160054882049962543533183634281158447265625`.
Independent logLik at that point: `-8.987226016005501168888778074` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.04e-07`.
Better feasible point: `[2.014271442794271838039321665, 6.318878967419974809881628237, 2.313632463022238787978460773]`; logLik `-8.987226016005498580758318441`.
Stationary local point: `[2.014271442794271838039321665, 6.318878967419974809881628237, 2.313632463022238787978460773]`; logLik `-8.987226016005498580758318441`. Verified coefficient relative error: `1.04e-07`.
Local information eigenvalue range: `[0.02237282077130486668421874316, 3.10138390813847701461751828]`; per-observation stationarity residual `2.610771189806905449363530204e-55`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 9414: betapr, face-inverse-gamma-100

Data: `[0.19750223697535038, 0.27364539295655965, 0.33688442435660365, 0.40044237016327694, 0.4695048599491591, 0.5486659010144386, 0.6439403714944437, 0.7650242268042201, 0.9302100346818517, 1.1809868624904873, 1.644194843858683, 3.127864215759038]`. Holds: `{'shape2': 3.2600098684669976}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9414` (run through the slot wrapper).
Returned: `{'shape1': '7.2764168067015635', 'shape2': '3.2600098684669976', 'scale': '0.25963004219312669'}`, status `0`, logLik `-8.260322277367119880864265724085271358489990234375`.
Independent logLik at that point: `-8.260322277367133315680904546` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.86e-08`.
Better feasible point: `[7.276416927396932311461431055, 3.260009868466997584590671977, 0.2596300373570572575927518645]`; logLik `-8.260322277367133234389716116`.
Stationary local point: `[7.276416927396932311461431055, 3.260009868466997584590671977, 0.2596300373570572575927518645]`; logLik `-8.260322277367133234389716116`. Verified coefficient relative error: `1.86e-08`.
Local information eigenvalue range: `[0.02169593269031288832253298097, 4.550366680292820995356311206]`; per-observation stationarity residual `4.503066796653582048721642797e-59`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 9440: gb2, face-trgamma-100

Data: `[0.5654263637999203, 0.7798720557157452, 0.9201899097971488, 1.036834620033989, 1.1433057974033751, 1.2461696589857656, 1.3500380685059752, 1.4594188587507675, 1.5802652438053586, 1.7228978694382755, 1.911639208868977, 2.25016305870004]`. Holds: `{'shape3': 0.7116483765738807}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9440` (run through the slot wrapper).
Returned: `{'shape1': '6.9743779240900867', 'shape2': '4.0853932507910757', 'shape3': '0.71164837657388069', 'scale': '2.61244542559057'}`, status `0`, logLik `-7.755156543768453758502801065333187580108642578125`.
Independent logLik at that point: `-7.755156543768457525275222653` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.78e-07`.
Better feasible point: `[6.974381955377375419988187131, 4.085393120216951643295743977, 0.7116483765738806921419268292, 2.612445881358628189835394412]`; logLik `-7.755156543768453980386955351`.
Stationary local point: `[6.974381955377375419988187131, 4.085393120216951643295743977, 0.7116483765738806921419268292, 2.612445881358628189835394412]`; logLik `-7.755156543768453980386955351`. Verified coefficient relative error: `5.78e-07`.
Local information eigenvalue range: `[0.001616122516530910971333608847, 12.21560524567618993006364561]`; per-observation stationarity residual `1.280499860311443101491896874e-45`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 9453: gb2, face-invtrgamma-100

Data: `[0.44441223765255433, 0.5231112625021179, 0.5804174569709318, 0.6328051597160669, 0.685204246884941, 0.7407198532606228, 0.8024589531523987, 0.8746566336593008, 0.9644739678611608, 1.0867321944667359, 1.2822616128772957, 1.768576889976525]`. Holds: `{'shape1': 0.5710330324328029}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9453` (run through the slot wrapper).
Returned: `{'shape1': '0.5710330324328029', 'shape2': '4.8765233053597621', 'shape3': '3.5432649100604574', 'scale': '0.45916102157535515'}`, status `0`, logLik `-2.557011192394646581504957794095389544963836669921875`.
Independent logLik at that point: `-2.557011192394709662659798432` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.07e-08`.
Better feasible point: `[0.5710330324328029005798157414, 4.876523300699848993521270746, 3.543264948027850832370543989, 0.4591610201895624990161530884]`; logLik `-2.557011192394709659258708672`.
Stationary local point: `[0.5710330324328029005798157414, 4.876523300699848993521270746, 3.543264948027850832370543989, 0.4591610201895624990161530884]`; logLik `-2.557011192394709659258708672`. Verified coefficient relative error: `1.07e-08`.
Local information eigenvalue range: `[0.00454005938609492712024995924, 10.571709943433489604402375]`; per-observation stationarity residual `3.077353121740806677911928506e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 9482: gb2, face-lnorm-100

Data: `[0.1769900087857597, 0.3167810802576964, 0.44422470119423696, 0.5781599645646766, 0.7274179021396269, 0.9007720890125155, 1.1101587318233443, 1.374725583545029, 1.7296251233046664, 2.2511130004964564, 3.1567541823726195, 5.65003644477167]`. Holds: `{'shape2': 1.123220989970799, 'shape3': 0.7116483765738807, 'scale': 1.0079365599415882}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9482` (run through the slot wrapper).
Returned: `{'shape1': '0.97440352988079115', 'shape2': '1.123220989970799', 'shape3': '0.71164837657388069', 'scale': '1.0079365599415882'}`, status `0`, logLik `-19.55229560653271647652218234725296497344970703125`.
Independent logLik at that point: `-19.55229560653277878595451177` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.94e-08`.
Better feasible point: `[0.9744034914908436732982517979, 1.123220989970798999735279722, 0.7116483765738806921419268292, 1.007936559941588150479674368]`; logLik `-19.5522956065327707662721066`.
Stationary local point: `[0.9744034914908436732982517979, 1.123220989970798999735279722, 0.7116483765738806921419268292, 1.007936559941588150479674368]`; logLik `-19.5522956065327707662721066`. Verified coefficient relative error: `3.94e-08`.
Local information eigenvalue range: `[0.8610900929048800218798368014, 0.8610900929048800218798368014]`; per-observation stationarity residual `7.22739147332275250022662712e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 9495: gb2, face-pareto-100

Data: `[1.030715288997118, 1.0796724770853794, 1.1350862928982455, 1.199999999713049, 1.2774975538131201, 1.372252499752477, 1.4917957452193464, 1.6492422502470643, 1.870064933937551, 2.2126906697502924, 2.8565713714171403, 4.947726750741194]`. Holds: `{'shape1': 0.5710330324328029, 'shape2': 1.123220989970799, 'scale': 1.0079365599415882}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9495` (run through the slot wrapper).
Returned: `{'shape1': '0.5710330324328029', 'shape2': '1.123220989970799', 'shape3': '1.3537499573841056', 'scale': '1.0079365599415882'}`, status `0`, logLik `-23.76544124446254357962970971129834651947021484375`.
Independent logLik at that point: `-23.76544124446260941851463513` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.51e-08`.
Better feasible point: `[0.5710330324328029005798157414, 1.123220989970798999735279722, 1.353749896269126486325563816, 1.007936559941588150479674368]`; logLik `-23.76544124446260053642723677`.
Stationary local point: `[0.5710330324328029005798157414, 1.123220989970798999735279722, 1.353749896269126486325563816, 1.007936559941588150479674368]`; logLik `-23.76544124446260053642723677`. Verified coefficient relative error: `4.51e-08`.
Local information eigenvalue range: `[0.7263490653784301749836484281, 0.7263490653784301749836484281]`; per-observation stationarity residual `2.207043207540088799902188416e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 9518: gb2, face-double-pareto-100

Data: `[0.26339769553123515, 0.4562181912566571, 0.5889751523246344, 0.6968847982831573, 0.7901930865937284, 0.8735913273001527, 0.9496965626030057, 1.0220156829711253, 1.1113791565452917, 1.2432875885353978, 1.4740804330726338, 2.1259918711975656]`. Holds: `{'shape2': 1.123220989970799}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9518` (run through the slot wrapper).
Returned: `{'shape1': '317.0693443326436', 'shape2': '1.123220989970799', 'shape3': '3.3230707833921076', 'scale': '56.564061503547848'}`, status `0`, logLik `-7.1442886918692050102208668249659240245819091796875`.
Independent logLik at that point: `-7.144288691869362680522078003` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `9.55e-06`.
Better feasible point: `[317.0723737123440537218637076, 1.123220989970798999735279722, 3.323070577238590250819773176, 56.56454728300938531570047738]`; logLik `-7.144288691869349739771669703`.
Stationary local point: `[317.0723737123440537218637076, 1.123220989970798999735279722, 3.323070577238590250819773176, 56.56454728300938531570047738]`; logLik `-7.144288691869349739771669703`. Verified coefficient relative error: `9.55e-06`.
Local information eigenvalue range: `[0.00001031453979188367472534726633, 10.90201270667783646363901533]`; per-observation stationarity residual `2.680729700231746919388985344e-65`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 9524: gb2, face-double-pareto-100

Data: `[0.26339769553123515, 0.4562181912566571, 0.5889751523246344, 0.6968847982831573, 0.7901930865937284, 0.8735913273001527, 0.9496965626030057, 1.0220156829711253, 1.1113791565452917, 1.2432875885353978, 1.4740804330726338, 2.1259918711975656]`. Holds: `{'scale': 1.0079365599415882}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9524` (run through the slot wrapper).
Returned: `{'shape1': '0.26644343225999967', 'shape2': '12.709872838195317', 'shape3': '0.17252264205569606', 'scale': '1.0079365599415882'}`, status `0`, logLik `-7.1541045085073680098730619647540152072906494140625`.
Independent logLik at that point: `-7.154104508507403277997992682` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.23e-08`.
Better feasible point: `[0.2664434211110198795749421746, 12.7098733375005732724708731, 0.1725226347506254813315575648, 1.007936559941588150479674368]`; logLik `-7.154104508507403252942122647`.
Stationary local point: `[0.2664434211110198795749421746, 12.7098733375005732724708731, 0.1725226347506254813315575648, 1.007936559941588150479674368]`; logLik `-7.154104508507403252942122647`. Verified coefficient relative error: `4.23e-08`.
Local information eigenvalue range: `[0.0008198700209016456726906679145, 1.609403055097876985741937569]`; per-observation stationarity residual `3.158143664698554841899132128e-53`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 9533: fpareto, face-trgamma-100

Data: `[0.5654263637999203, 0.7798720557157452, 0.9201899097971488, 1.036834620033989, 1.1433057974033751, 1.2461696589857656, 1.3500380685059752, 1.4594188587507675, 1.5802652438053586, 1.7228978694382755, 1.911639208868977, 2.25016305870004]`. Holds: `{'shape1': 0.3, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9533` (run through the slot wrapper).
Returned: `{'shape1': '0.29999999999999999', 'shape2': '17.132746952262924', 'shape3': '0.15577338859982556', 'scale': '1.5104482188570743', 'min': '0'}`, status `0`, logLik `-8.209988477740349566147415316663682460784912109375`.
Independent logLik at that point: `-8.20998847774041155156043991` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.67e-08`.
Better feasible point: `[0.2999999999999999888977697537, 17.13274723676607056601528685, 0.1557733844452773206136312552, 1.510448227695318439639353911, 0.0]`; logLik `-8.209988477740411200918574133`.
Stationary local point: `[0.2999999999999999888977697537, 17.13274723676607056601528685, 0.1557733844452773206136312552, 1.510448227695318439639353911, 0.0]`; logLik `-8.209988477740411200918574133`. Verified coefficient relative error: `2.67e-08`.
Local information eigenvalue range: `[0.05637113212131028889507320163, 8.287740205664045330534539551]`; per-observation stationarity residual `4.540610500463964433580221289e-59`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9541: fpareto, face-trgamma-100

Data: `[0.5654263637999203, 0.7798720557157452, 0.9201899097971488, 1.036834620033989, 1.1433057974033751, 1.2461696589857656, 1.3500380685059752, 1.4594188587507675, 1.5802652438053586, 1.7228978694382755, 1.911639208868977, 2.25016305870004]`. Holds: `{'shape1': 0.3, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9541` (run through the slot wrapper).
Returned: `{'shape1': '0.29999999999999999', 'shape2': '8.4779802909673823', 'shape3': '0.59375909828721296', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-9.125530037845972941568106762133538722991943359375`.
Independent logLik at that point: `-9.125530037846010051879234027` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.51e-08`.
Better feasible point: `[0.2999999999999999888977697537, 8.477980328159057842180459801, 0.5937590893214489376107561523, 1.008784292115731506811471263, 0.0]`; logLik `-9.125530037846009310069040381`.
Stationary local point: `[0.2999999999999999888977697537, 8.477980328159057842180459801, 0.5937590893214489376107561523, 1.008784292115731506811471263, 0.0]`; logLik `-9.125530037846009310069040381`. Verified coefficient relative error: `1.51e-08`.
Local information eigenvalue range: `[0.487922787378125676197088933, 1.347179323379001172305979556]`; per-observation stationarity residual `9.136845130727226121930715738e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9552: fpareto, face-invtrgamma-100

Data: `[0.44441223765255433, 0.5231112625021179, 0.5804174569709318, 0.6328051597160669, 0.685204246884941, 0.7407198532606228, 0.8024589531523987, 0.8746566336593008, 0.9644739678611608, 1.0867321944667359, 1.2822616128772957, 1.768576889976525]`. Holds: `{'shape3': 0.4, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9552` (run through the slot wrapper).
Returned: `{'shape1': '0.20724939681906313', 'shape2': '13.197999139864987', 'shape3': '0.40000000000000002', 'scale': '0.66026512831617223', 'min': '0'}`, status `0`, logLik `-2.897501580617624750146887890878133475780487060546875`.
Independent logLik at that point: `-2.897501580617641265346104865` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.93e-08`.
Better feasible point: `[0.207249392817493623691646134, 13.19799929346252137113650403, 0.4000000000000000222044604925, 0.660265125472300749615203603, 0.0]`; logLik `-2.897501580617641086119433622`.
Stationary local point: `[0.207249392817493623691646134, 13.19799929346252137113650403, 0.4000000000000000222044604925, 0.660265125472300749615203603, 0.0]`; logLik `-2.897501580617641086119433622`. Verified coefficient relative error: `1.93e-08`.
Local information eigenvalue range: `[0.05593976981066395964886252151, 8.291930009314498297495079237]`; per-observation stationarity residual `2.27919007525537543138966523e-60`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9558: fpareto, face-invtrgamma-100

Data: `[0.44441223765255433, 0.5231112625021179, 0.5804174569709318, 0.6328051597160669, 0.685204246884941, 0.7407198532606228, 0.8024589531523987, 0.8746566336593008, 0.9644739678611608, 1.0867321944667359, 1.2822616128772957, 1.768576889976525]`. Holds: `{'shape2': 1.9802932530684678, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9558` (run through the slot wrapper).
Returned: `{'shape1': '4.916755581199336', 'shape2': '1.9802932530684678', 'shape3': '3.3053031290779513', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-2.877443322671062286843834954197518527507781982421875`.
Independent logLik at that point: `-2.877443322671100193292962447` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.01e-08`.
Better feasible point: `[4.916755551972750483421862465, 1.980293253068467818067688313, 3.305303095785982117498551394, 1.008784292115731506811471263, 0.0]`; logLik `-2.877443322671099724017821783`.
Stationary local point: `[4.916755551972750483421862465, 1.980293253068467818067688313, 3.305303095785982117498551394, 1.008784292115731506811471263, 0.0]`; logLik `-2.877443322671099724017821783`. Verified coefficient relative error: `1.01e-08`.
Local information eigenvalue range: `[0.2806578709599179354255724812, 4.485539047102455803037157564]`; per-observation stationarity residual `2.770125317170820870273425302e-64`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9559: fpareto, face-invtrgamma-100

Data: `[0.44441223765255433, 0.5231112625021179, 0.5804174569709318, 0.6328051597160669, 0.685204246884941, 0.7407198532606228, 0.8024589531523987, 0.8746566336593008, 0.9644739678611608, 1.0867321944667359, 1.2822616128772957, 1.768576889976525]`. Holds: `{'shape1': 0.3, 'shape2': 1.9802932530684678, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9559` (run through the slot wrapper).
Returned: `{'shape1': '0.29999999999999999', 'shape2': '1.9802932530684678', 'shape3': '0.49639527593076321', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-15.4217738040097973595266012125648558139801025390625`.
Independent logLik at that point: `-15.42177380400986361784376443` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.34e-08`.
Better feasible point: `[0.2999999999999999888977697537, 1.980293253068467818067688313, 0.4963952692916566543993040792, 1.008784292115731506811471263, 0.0]`; logLik `-15.42177380400986290892555429`.
Stationary local point: `[0.2999999999999999888977697537, 1.980293253068467818067688313, 0.4963952692916566543993040792, 1.008784292115731506811471263, 0.0]`; logLik `-15.42177380400986290892555429`. Verified coefficient relative error: `1.34e-08`.
Local information eigenvalue range: `[0.6605122668162488865724915911, 0.6605122668162488865724915911]`; per-observation stationarity residual `2.101481194166245244278164995e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9565: fpareto, face-lnorm-100

Data: `[0.1769900087857597, 0.3167810802576964, 0.44422470119423696, 0.5781599645646766, 0.7274179021396269, 0.9007720890125155, 1.1101587318233443, 1.374725583545029, 1.7296251233046664, 2.2511130004964564, 3.1567541823726195, 5.65003644477167]`. Holds: `{'shape1': 0.3, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9565` (run through the slot wrapper).
Returned: `{'shape1': '0.29999999999999999', 'shape2': '4.338806016706334', 'shape3': '0.34633550480153752', 'scale': '0.9074325013287281', 'min': '0'}`, status `0`, logLik `-17.0241095251071072880222345702350139617919921875`.
Independent logLik at that point: `-17.02410952510712696153098853` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.25e-07`.
Better feasible point: `[0.2999999999999999888977697537, 4.338805789890130568090430447, 0.3463355481611663945100110279, 0.9074324239078826731480192902, 0.0]`; logLik `-17.02410952510712013524468497`.
Stationary local point: `[0.2999999999999999888977697537, 4.338805789890130568090430447, 0.3463355481611663945100110279, 0.9074324239078826731480192902, 0.0]`; logLik `-17.02410952510712013524468497`. Verified coefficient relative error: `1.25e-07`.
Local information eigenvalue range: `[0.04355725103233805257149516783, 1.754822297789143712665980451]`; per-observation stationarity residual `2.74532563363768174174843655e-53`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9567: fpareto, face-lnorm-100

Data: `[0.1769900087857597, 0.3167810802576964, 0.44422470119423696, 0.5781599645646766, 0.7274179021396269, 0.9007720890125155, 1.1101587318233443, 1.374725583545029, 1.7296251233046664, 2.2511130004964564, 3.1567541823726195, 5.65003644477167]`. Holds: `{'shape1': 0.3, 'shape2': 1.9802932530684678, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9567` (run through the slot wrapper).
Returned: `{'shape1': '0.29999999999999999', 'shape2': '1.9802932530684678', 'shape3': '8.3892510051529534', 'scale': '0.092909963374363955', 'min': '0'}`, status `0`, logLik `-18.270973671996376452852928196080029010772705078125`.
Independent logLik at that point: `-18.27097367199642318186105367` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.92e-07`.
Better feasible point: `[0.2999999999999999888977697537, 1.980293253068467818067688313, 8.3892568069919409555598823, 0.09290992702966935378610691939, 0.0]`; logLik `-18.27097367199641998211583023`.
Stationary local point: `[0.2999999999999999888977697537, 1.980293253068467818067688313, 8.3892568069919409555598823, 0.09290992702966935378610691939, 0.0]`; logLik `-18.27097367199641998211583023`. Verified coefficient relative error: `6.92e-07`.
Local information eigenvalue range: `[0.0008445703104278884378134420715, 1.338746247844838373510507496]`; per-observation stationarity residual `3.193372675664330374877177928e-44`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9610: fpareto, face-power-100

Data: `[0.2021130208636108, 0.3500700210070024, 0.45193845378886705, 0.5347407899331234, 0.6063390625908325, 0.6703330554498698, 0.728728860162672, 0.7827803638567952, 0.8333333335326049, 0.8809902879248711, 0.926206809216384, 0.9702000258218699]`. Holds: `{'shape2': 1.9802932530684678, 'shape3': 0.4, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9610` (run through the slot wrapper).
Returned: `{'shape1': '1.3020895738396188', 'shape2': '1.9802932530684678', 'shape3': '0.40000000000000002', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-7.75113602502796705806531463167630136013031005859375`.
Independent logLik at that point: `-7.751136025028046613482351744` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.86e-08`.
Better feasible point: `[1.302089549610017072437536188, 1.980293253068467818067688313, 0.4000000000000000222044604925, 1.008784292115731506811471263, 0.0]`; logLik `-7.751136025028045416620387903`.
Stationary local point: `[1.302089549610017072437536188, 1.980293253068467818067688313, 0.4000000000000000222044604925, 1.008784292115731506811471263, 0.0]`; logLik `-7.751136025028045416620387903`. Verified coefficient relative error: `1.86e-08`.
Local information eigenvalue range: `[0.5760789338833099429369010394, 0.5760789338833099429369010394]`; per-observation stationarity residual `1.982779855030102558749143194e-62`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9616: fpareto, face-double-pareto-100

Data: `[0.26339769553123515, 0.4562181912566571, 0.5889751523246344, 0.6968847982831573, 0.7901930865937284, 0.8735913273001527, 0.9496965626030057, 1.0220156829711253, 1.1113791565452917, 1.2432875885353978, 1.4740804330726338, 2.1259918711975656]`. Holds: `{'shape3': 0.4, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9616` (run through the slot wrapper).
Returned: `{'shape1': '0.69843599045567772', 'shape2': '5.8287781323724248', 'shape3': '0.40000000000000002', 'scale': '1.0698976225531649', 'min': '0'}`, status `0`, logLik `-7.1540534017355827955952918273396790027618408203125`.
Independent logLik at that point: `-7.154053401735593496118763453` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.07e-08`.
Better feasible point: `[0.6984359979093820216446099953, 5.828778113502000223914778986, 0.4000000000000000222044604925, 1.069897626443667022136645356, 0.0]`; logLik `-7.154053401735593433276560108`.
Stationary local point: `[0.6984359979093820216446099953, 5.828778113502000223914778986, 0.4000000000000000222044604925, 1.069897626443667022136645356, 0.0]`; logLik `-7.154053401735593433276560108`. Verified coefficient relative error: `1.07e-08`.
Local information eigenvalue range: `[0.07556555635681866641685258863, 5.067087924688808119961020219]`; per-observation stationarity residual `1.622548147194121050427178714e-62`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9620: fpareto, face-double-pareto-100

Data: `[0.26339769553123515, 0.4562181912566571, 0.5889751523246344, 0.6968847982831573, 0.7901930865937284, 0.8735913273001527, 0.9496965626030057, 1.0220156829711253, 1.1113791565452917, 1.2432875885353978, 1.4740804330726338, 2.1259918711975656]`. Holds: `{'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9620` (run through the slot wrapper).
Returned: `{'shape1': '0.26962948585974894', 'shape2': '12.585168479986185', 'shape3': '0.17422019524308538', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-7.1539067409872583169772042310796678066253662109375`.
Independent logLik at that point: `-7.153906740987328120764694354` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.1e-08`.
Better feasible point: `[0.2696294749264892041983372148, 12.58516895870052535672514669, 0.1742201880919112035236662187, 1.008784292115731506811471263, 0.0]`; logLik `-7.15390674098732809683153961`.
Stationary local point: `[0.2696294749264892041983372148, 12.58516895870052535672514669, 0.1742201880919112035236662187, 1.008784292115731506811471263, 0.0]`; logLik `-7.15390674098732809683153961`. Verified coefficient relative error: `4.1e-08`.
Local information eigenvalue range: `[0.0008341286069141234864116766987, 1.610830695202795915627688377]`; per-observation stationarity residual `2.351305640282213893638859906e-53`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9680: zmnbinom, face-Poisson-100

Data: `[1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5]`. Holds: `{}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9680` (run through the slot wrapper).
Returned: `{'size': '5.7353723218502228', 'prob': '0.77511388544820692', 'p0': '0'}`, status `0`, logLik `-17.684784790596353332148282788693904876708984375`.
Independent logLik at that point: `-17.68478479059635674486905341` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.45e-08`.
Better feasible point: `[5.735371951688734394127874977, 0.775113875151507350519748035, 0.0]`; logLik `-17.68478479059635652783915121`.
Stationary local point: `[5.735371951688734394127874977, 0.775113875151507350519748035, 0.0]`; logLik `-17.68478479059635652783915121`. Verified coefficient relative error: `6.45e-08`.
Local information eigenvalue range: `[0.00833071180046703457000975443, 21.12848144583860905755741716]`; per-observation stationarity residual `3.661000027386657288322926301e-53`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9682: zmnbinom, face-Poisson-100

Data: `[1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5]`. Holds: `{'prob': 0.41499877689089204}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9682` (run through the slot wrapper).
Returned: `{'size': '0.84558425364009249', 'prob': '0.41499877689089204', 'p0': '0'}`, status `0`, logLik `-18.0198270550488786057030665688216686248779296875`.
Independent logLik at that point: `-18.0198270550488817270210382` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.72e-08`.
Better feasible point: `[0.8455842137641897990602370739, 0.4149987768908920404697937556, 0.0]`; logLik `-18.01982705504887896069593627`.
Stationary local point: `[0.8455842137641897990602370739, 0.4149987768908920404697937556, 0.0]`; logLik `-18.01982705504887896069593627`. Verified coefficient relative error: `4.72e-08`.
Local information eigenvalue range: `[0.2073216663047825151454130637, 0.2073216663047825151454130637]`; per-observation stationarity residual `2.132094989891671353358473829e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9684: zmnbinom, face-Poisson-100

Data: `[1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5]`. Holds: `{'p0': 0.314}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9684` (run through the slot wrapper).
Returned: `{'size': '5.7353723218502228', 'prob': '0.77511388544820692', 'p0': '0.314'}`, status `0`, logLik `-22.20731660567137311090846196748316287994384765625`.
Independent logLik at that point: `-22.20731660567137869602223467` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.45e-08`.
Better feasible point: `[5.735371951688734394127874977, 0.775113875151507350519748035, 0.3140000000000000013322676296]`; logLik `-22.20731660567137847899233246`.
Stationary local point: `[5.735371951688734394127874977, 0.775113875151507350519748035, 0.3140000000000000013322676296]`; logLik `-22.20731660567137847899233246`. Verified coefficient relative error: `6.45e-08`.
Local information eigenvalue range: `[0.00833071180046703457000975443, 21.12848144583860905755741716]`; per-observation stationarity residual `3.661000027386657288322926301e-53`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9686: zmnbinom, face-Poisson-100

Data: `[1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5]`. Holds: `{'prob': 0.41499877689089204, 'p0': 0.314}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9686` (run through the slot wrapper).
Returned: `{'size': '0.84558425364009249', 'prob': '0.41499877689089204', 'p0': '0.314'}`, status `0`, logLik `-22.54235887012389838446324574761092662811279296875`.
Independent logLik at that point: `-22.54235887012390367817421946` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.72e-08`.
Better feasible point: `[0.8455842137641897990602370739, 0.4149987768908920404697937556, 0.3140000000000000013322676296]`; logLik `-22.54235887012390091184911753`.
Stationary local point: `[0.8455842137641897990602370739, 0.4149987768908920404697937556, 0.3140000000000000013322676296]`; logLik `-22.54235887012390091184911753`. Verified coefficient relative error: `4.72e-08`.
Local information eigenvalue range: `[0.2073216663047825151454130637, 0.2073216663047825151454130637]`; per-observation stationarity residual `2.132094989891671353358473829e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9688: ztnbinom, face-Poisson-100

Data: `[1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5]`. Holds: `{}`.
Call: `route_ztnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9688` (run through the slot wrapper).
Returned: `{'size': '5.7353723218502228', 'prob': '0.77511388544820692'}`, status `0`, logLik `-17.684784790596353332148282788693904876708984375`.
Independent logLik at that point: `-17.68478479059635674486905341` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.45e-08`.
Better feasible point: `[5.735371951688734394127874977, 0.775113875151507350519748035]`; logLik `-17.68478479059635652783915121`.
Stationary local point: `[5.735371951688734394127874977, 0.775113875151507350519748035]`; logLik `-17.68478479059635652783915121`. Verified coefficient relative error: `6.45e-08`.
Local information eigenvalue range: `[0.00833071180046703457000975443, 21.12848144583860905755741716]`; per-observation stationarity residual `3.661000027386657288322926301e-53`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ztnbinom/mle.hpp:34` (step stop): `if(maxit < std::ceil(std::log(std::min(tol,0.5))/std::log(0.6180339887498948482)))`. Execution-path causation remains open.

## Case 9690: ztnbinom, face-Poisson-100

Data: `[1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5]`. Holds: `{'prob': 0.4269728320855304}`.
Call: `route_ztnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9690` (run through the slot wrapper).
Returned: `{'size': '0.90873813000253367', 'prob': '0.42697283208553038'}`, status `0`, logLik `-17.994615690098225257997910375706851482391357421875`.
Independent logLik at that point: `-17.99461569009822897226511966` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.5e-08`.
Better feasible point: `[0.9087380891095489083401484499, 0.4269728320855303849157280638]`; logLik `-17.99461569009822623847061457`.
Stationary local point: `[0.9087380891095489083401484499, 0.4269728320855303849157280638]`; logLik `-17.99461569009822623847061457`. Verified coefficient relative error: `4.5e-08`.
Local information eigenvalue range: `[0.2250062082767425995510851612, 0.2250062082767425995510851612]`; per-observation stationarity residual `2.109724269844952828665389806e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ztnbinom/mle.hpp:34` (step stop): `if(maxit < std::ceil(std::log(std::min(tol,0.5))/std::log(0.6180339887498948482)))`. Execution-path causation remains open.

## Case 9719: emg, face-translated-exponential-100

Data: `[0.025242970457573472, 0.12202789882076255, 0.22549267316650606, 0.3393552634647486, 0.46681723560609184, 0.612058138330268, 0.7812048921057158, 0.98401564665137, 1.2376289041196136, 1.5767380959288442, 2.0909450354835957, 3.195370474309169]`. Holds: `{'mu': 0.17576157248036894, 'lambda': 1.209185930245896}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9719` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.20553413323028549', 'lambda': '1.209185930245896'}`, status `0`, logLik `-13.1021879433973520434619786101393401622772216796875`.
Independent logLik at that point: `-13.10218794339735602891783628` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.4e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.2055341381732845191967395779, 1.20918593024589604567609058]`; logLik `-13.10218794339735522891358299`.
Stationary local point: `[0.1757615724803689438715537108, 0.2055341381732845191967395779, 1.20918593024589604567609058]`; logLik `-13.10218794339735522891358299`. Verified coefficient relative error: `2.4e-08`.
Local information eigenvalue range: `[0.2305302360022411771262591711, 0.2305302360022411771262591711]`; per-observation stationarity residual `1.632768067086992540344267542e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 9732: ged, face-uniform-100

Data: `[-1.5873304850778167, -1.29872373106068, -1.0101184574916384, -0.7215131839225987, -0.4329079103535595, -0.14430263678451988, 0.1443026367845197, 0.4329079103535595, 0.7215131839225988, 1.0101184574916382, 1.29872373106068, 1.5873304850778167]`. Holds: `{'sd': 1.0178559902092952}`.
Call: `route_ged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9732` (run through the slot wrapper).
Returned: `{'mean': '-1.1102230246251565e-16', 'sd': '1.0178559902092952', 'nu': '57.779698753987617'}`, status `0`, logLik `-15.1160001459259358824738228577189147472381591796875`.
Independent logLik at that point: `-15.11600014592593889374550664` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.3e-07`.
Better feasible point: `[-1.247748218212825807233776386e-27, 1.01785599020929518054856544, 57.77969124078393183987392016]`; logLik `-15.11600014592593843931858104`.
Stationary local point: `[-1.247748218212825807233776386e-27, 1.01785599020929518054856544, 57.77969124078393183987392016]`; logLik `-15.11600014592593843931858104`. Verified coefficient relative error: `1.3e-07`.
Local information eigenvalue range: `[0.004479331263794630071681805097, 3.01786281367944818411420472]`; per-observation stationarity residual `7.280005287253445664942311822e-54`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp:219` (step stop): `inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {`. Execution-path causation remains open.

## Case 9733: ged, face-uniform-100

Data: `[-1.5873304850778167, -1.29872373106068, -1.0101184574916384, -0.7215131839225987, -0.4329079103535595, -0.14430263678451988, 0.1443026367845197, 0.4329079103535595, 0.7215131839225988, 1.0101184574916382, 1.29872373106068, 1.5873304850778167]`. Holds: `{'mean': -0.017698887677594307, 'sd': 1.0178559902092952}`.
Call: `route_ged(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9733` (run through the slot wrapper).
Returned: `{'mean': '-0.017698887677594307', 'sd': '1.0178559902092952', 'nu': '60.348279179994449'}`, status `0`, logLik `-15.11653182782301740871844231151044368743896484375`.
Independent logLik at that point: `-15.11653182782302092944623735` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.72e-07`.
Better feasible point: `[-0.01769888767759430700521328106, 1.01785599020929518054856544, 60.34826880548202220283073216]`; logLik `-15.11653182782302021741757596`.
Stationary local point: `[-0.01769888767759430700521328106, 1.01785599020929518054856544, 60.34826880548202220283073216]`; logLik `-15.11653182782302021741757596`. Verified coefficient relative error: `1.72e-07`.
Local information eigenvalue range: `[0.004015511012639903457572819105, 0.004015511012639903457572819105]`; per-observation stationarity residual `6.140962897144174263838544584e-53`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ged/mle.hpp:219` (step stop): `inline Maximum maximize(F f,const std::vector<double>& grid,double tol,int maxit) {`. Execution-path causation remains open.

## Case 9747: foldnorm, face-collapse-100

Data: `[0.9826833560387775, 0.98849650619624, 0.9918778219850009, 0.9945147771730191, 0.9968136063603562, 0.9989536654438592, 1.0010463345561407, 1.0031863936396437, 1.005485222826981, 1.0081221780149991, 1.0115034938037601, 1.0173166439612225]`. Holds: `{'mu': 2.0248532542607856}`.
Call: `route_foldnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9747` (run through the slot wrapper).
Returned: `{'mu': '2.0248532542607856', 'sigma': '1.4023638648326486'}`, status `0`, logLik `-16.84906031731693332176291733048856258392333984375`.
Independent logLik at that point: `-16.84906031731693461713920703` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.39e-08`.
Better feasible point: `[2.024853254260785551110757297, 1.402363845347115363507478236]`; logLik `-16.84906031731693427135425507`.
Stationary local point: `[2.024853254260785551110757297, 1.402363845347115363507478236]`; logLik `-16.84906031731693427135425507`. Verified coefficient relative error: `1.39e-08`.
Local information eigenvalue range: `[0.298505043619005332369632663, 0.298505043619005332369632663]`; per-observation stationarity residual `2.833981412467870005281684073e-69`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/foldnorm/mle.hpp:28` (step stop): `for (;k<maxit && b-a>tol;++k) {`. Execution-path causation remains open.

## Case 9794: weibull, face-point-mass-1e+06

Data: `[0.9999968431554891, 0.9999979865833489, 0.9999985459196024, 0.9999989353272393, 0.9999992449854225, 0.999999510780191, 0.9999997517419298, 0.9999999806431116, 1.0000002087555044, 1.000000450193751, 1.000000732099636, 1.000001156269675]`. Holds: `{}`.
Call: `route_weibull(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9794` (run through the slot wrapper).
Returned: `{'shape': '1058060.6806440772', 'scale': '0.99999999315458854'}`, status `0`, logLik `147.510434735673925388255156576633453369140625`.
Independent logLik at that point: `147.5104347356739317186908177` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.27e-08`.
Better feasible point: `[1058060.704629906376787138402, 0.9999999931545975168638585517]`; logLik `147.5104347356739366485509958`.
Stationary local point: `[1058060.704629906376787138402, 0.9999999931545975168638585517]`; logLik `147.5104347356739366485509958`. Verified coefficient relative error: `2.27e-08`.
Local information eigenvalue range: `[1.598800659013833655662553249, 1119492454682.090780273369057]`; per-observation stationarity residual `1.067146089111908930442058438e-56`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/weibull/mle.hpp:29`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9798: invweibull, face-point-mass-1e+06

Data: `[0.999998843731662, 0.9999992679008999, 0.9999995498064518, 0.9999997912445391, 1.0000000193568888, 1.0000002482581318, 1.0000004892200485, 1.0000007550151475, 1.0000010646738942, 1.000001454082512, 1.000002013420705, 1.0000031568544765]`. Holds: `{}`.
Call: `route_invweibull(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9798` (run through the slot wrapper).
Returned: `{'shape': '1058060.6806370516', 'scale': '1.0000000068454116'}`, status `0`, logLik `147.510421428520743347689858637750148773193359375`.
Independent logLik at that point: `147.5104214285207243521615024` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.27e-08`.
Better feasible point: `[1058060.704622881360516096488, 1.000000006845402511663000996]`; logLik `147.5104214285207292820293293`.
Stationary local point: `[1058060.704622881360516096488, 1.000000006845402511663000996]`; logLik `147.5104214285207292820293293`. Verified coefficient relative error: `2.27e-08`.
Local information eigenvalue range: `[1.598800659022512038893375996, 1119492454667.26537532220547]`; per-observation stationarity residual `1.067732362233599487900095763e-56`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/invweibull/mle.hpp:29`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 9873: betapr, face-inverse-gamma-1e+06

Data: `[0.2014600286836266, 0.27723653210969484, 0.3401672573072183, 0.40341361135827547, 0.47213636849724944, 0.5509070864656758, 0.6457109741805451, 0.7661959829291036, 0.9305639893310217, 1.1800982590528828, 1.64100962152598, 3.1173189438299365]`. Holds: `{'shape1': 2.014271442794272}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9873` (run through the slot wrapper).
Returned: `{'shape1': '2.0142714427942718', 'shape2': '6.5186292170747508', 'scale': '2.4018793661840694'}`, status `0`, logLik `-8.970408025760502113143957103602588176727294921875`.
Independent logLik at that point: `-8.970408025760507887665124121` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `8.58e-08`.
Better feasible point: `[2.014271442794271838039321665, 6.518628728359413438429094706, 2.401879160098285563737329318]`; logLik `-8.970408025760506204221790833`.
Stationary local point: `[2.014271442794271838039321665, 6.518628728359413438429094706, 2.401879160098285563737329318]`; logLik `-8.970408025760506204221790833`. Verified coefficient relative error: `8.58e-08`.
Local information eigenvalue range: `[0.02149623132028908294133768799, 3.122238629309165081495009151]`; per-observation stationarity residual `5.454242418210536764858118037e-56`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 9874: betapr, face-inverse-gamma-1e+06

Data: `[0.2014600286836266, 0.27723653210969484, 0.3401672573072183, 0.40341361135827547, 0.47213636849724944, 0.5509070864656758, 0.6457109741805451, 0.7661959829291036, 0.9305639893310217, 1.1800982590528828, 1.64100962152598, 3.1173189438299365]`. Holds: `{'shape2': 3.2600098684669976}`.
Call: `route_betapr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9874` (run through the slot wrapper).
Returned: `{'shape1': '7.7706246151284724', 'shape2': '3.2600098684669976', 'scale': '0.24309161250627662'}`, status `0`, logLik `-8.2058539472061848840667153126560151576995849609375`.
Independent logLik at that point: `-8.205853947206196749337093035` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.38e-07`.
Better feasible point: `[7.770625573770624833860766129, 3.260009868466997584590671977, 0.243091579046222784690620999]`; logLik `-8.205853947206192687698745931`.
Stationary local point: `[7.770625573770624833860766129, 3.260009868466997584590671977, 0.243091579046222784690620999]`; logLik `-8.205853947206192687698745931`. Verified coefficient relative error: `1.38e-07`.
Local information eigenvalue range: `[0.01974532747500311456377360282, 4.636633287325344285182069871]`; per-observation stationarity residual `4.218101641132578648292609491e-52`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/betapr/mle.hpp:130`: Held-scale logLik reconstructs large cancelling shape terms and calls the direct log-beta difference at special/special.hpp:83–87. Other subsets also require distinguishing default stopping accuracy from objective errors.

## Case 9900: gb2, face-trgamma-1e+06

Data: `[0.5663819190107332, 0.7806285600112793, 0.9205362919909924, 1.0366374131020288, 1.1424312247159791, 1.2444599265347813, 1.3472891755611596, 1.4553460529279152, 1.574434966086264, 1.7145641777078129, 1.8992177964285772, 2.227950585690051]`. Holds: `{'shape3': 0.7116483765738807}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9900` (run through the slot wrapper).
Returned: `{'shape1': '8.4497454126542912', 'shape2': '4.0913064607306362', 'shape3': '0.71164837657388069', 'scale': '2.737221208826865'}`, status `0`, logLik `-7.60483462456255043804276283481158316135406494140625`.
Independent logLik at that point: `-7.604834624562575857041630358` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.02e-07`.
Better feasible point: `[8.449747115729121622303900531, 4.091306422932566129837486219, 0.7116483765738806921419268292, 2.737221370934398919620748126]`; logLik `-7.604834624562575560222396267`.
Stationary local point: `[8.449747115729121622303900531, 4.091306422932566129837486219, 0.7116483765738806921419268292, 2.737221370934398919620748126]`; logLik `-7.604834624562575560222396267`. Verified coefficient relative error: `2.02e-07`.
Local information eigenvalue range: `[0.001118773687903906785764047226, 13.09700082951480991656042267]`; per-observation stationarity residual `6.043124329023126876353117889e-49`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 9913: gb2, face-invtrgamma-1e+06

Data: `[0.44884298889882046, 0.5265325556028752, 0.5832385938080731, 0.6351484955176038, 0.687121800336192, 0.742231154335141, 0.8035614314914231, 0.8753262151501601, 0.9646574466260144, 1.0863232755735668, 1.2810189778164804, 1.7655930855749113]`. Holds: `{'shape1': 0.5710330324328029}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9913` (run through the slot wrapper).
Returned: `{'shape1': '0.5710330324328029', 'shape2': '4.8848115114539521', 'shape3': '3.9608735036130627', 'scale': '0.44881321616667397'}`, status `0`, logLik `-2.46450408147095156863315423834137618541717529296875`.
Independent logLik at that point: `-2.464504081470972634689610156` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `5.16e-07`.
Better feasible point: `[0.5710330324328029005798157414, 4.884811309925300892770115495, 3.960875546257218223104732044, 0.4488131524614794276832862016]`; logLik `-2.464504081470966276047383891`.
Stationary local point: `[0.5710330324328029005798157414, 4.884811309925300892770115495, 3.960875546257218223104732044, 0.4488131524614794276832862016]`; logLik `-2.464504081470966276047383891`. Verified coefficient relative error: `5.16e-07`.
Local information eigenvalue range: `[0.00368199996639919617268142437, 11.06972119840436826940375308]`; per-observation stationarity residual `1.324183863179514753490774497e-45`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 9942: gb2, face-lnorm-1e+06

Data: `[0.17698958392421094, 0.3165261874667128, 0.4438725877326036, 0.5778030449797549, 0.7271377620782987, 0.9006546037111516, 1.110303545753946, 1.3752552159328302, 1.73069354460577, 2.252898754366237, 3.159296259192437, 5.6500500076221565]`. Holds: `{'shape2': 1.123220989970799, 'shape3': 0.7116483765738807, 'scale': 1.0079365599415882}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9942` (run through the slot wrapper).
Returned: `{'shape1': '0.97429430594506861', 'shape2': '1.123220989970799', 'shape3': '0.71164837657388069', 'scale': '1.0079365599415882'}`, status `0`, logLik `-19.55429964662926778373730485327541828155517578125`.
Independent logLik at that point: `-19.55429964662933642635142577` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.09e-08`.
Better feasible point: `[0.9742942855935845965400823054, 1.123220989970798999735279722, 0.7116483765738806921419268292, 1.007936559941588150479674368]`; logLik `-19.55429964662933417202437621`.
Stationary local point: `[0.9742942855935845965400823054, 1.123220989970798999735279722, 0.7116483765738806921419268292, 1.007936559941588150479674368]`; logLik `-19.55429964662933417202437621`. Verified coefficient relative error: `2.09e-08`.
Local information eigenvalue range: `[0.8611004990167095043681873901, 0.8611004990167095043681873901]`; per-observation stationarity residual `4.512366013843746894726495623e-62`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 9993: fpareto, face-trgamma-1e+06

Data: `[0.5663819190107332, 0.7806285600112793, 0.9205362919909924, 1.0366374131020288, 1.1424312247159791, 1.2444599265347813, 1.3472891755611596, 1.4553460529279152, 1.574434966086264, 1.7145641777078129, 1.8992177964285772, 2.227950585690051]`. Holds: `{'shape1': 0.3, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 9993` (run through the slot wrapper).
Returned: `{'shape1': '0.29999999999999999', 'shape2': '17.547373797074389', 'shape3': '0.15227108589829211', 'scale': '1.5129507291603974', 'min': '0'}`, status `0`, logLik `-8.066098541115838571613494423218071460723876953125`.
Independent logLik at that point: `-8.066098541115874472589075047` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `8.39e-08`.
Better feasible point: `[0.2999999999999999888977697537, 17.54737472205413255346653302, 0.1522710731210744422135878389, 1.512950756536814885124802549, 0.0]`; logLik `-8.066098541115871031763500076`.
Stationary local point: `[0.2999999999999999888977697537, 17.54737472205413255346653302, 0.1522710731210744422135878389, 1.512950756536814885124802549, 0.0]`; logLik `-8.066098541115871031763500076`. Verified coefficient relative error: `8.39e-08`.
Local information eigenvalue range: `[0.05567436865594515706647481668, 8.488465341093781968204712538]`; per-observation stationarity residual `4.367505621814530417096381379e-55`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10012: fpareto, face-invtrgamma-1e+06

Data: `[0.44884298889882046, 0.5265325556028752, 0.5832385938080731, 0.6351484955176038, 0.687121800336192, 0.742231154335141, 0.8035614314914231, 0.8753262151501601, 0.9646574466260144, 1.0863232755735668, 1.2810189778164804, 1.7655930855749113]`. Holds: `{'shape3': 0.4, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10012` (run through the slot wrapper).
Returned: `{'shape1': '0.20253671609224264', 'shape2': '13.513859583999764', 'shape3': '0.40000000000000002', 'scale': '0.65913690235758005', 'min': '0'}`, status `0`, logLik `-2.81233353448183986955655200290493667125701904296875`.
Independent logLik at that point: `-2.812333534481865434277186548` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.32e-08`.
Better feasible point: `[0.202536707343108723367179494, 13.51385993924285314945741241, 0.4000000000000000222044604925, 0.659136896119785204621157686, 0.0]`; logLik `-2.812333534481864543390186459`.
Stationary local point: `[0.202536707343108723367179494, 13.51385993924285314945741241, 0.4000000000000000222044604925, 0.659136896119785204621157686, 0.0]`; logLik `-2.812333534481864543390186459`. Verified coefficient relative error: `4.32e-08`.
Local information eigenvalue range: `[0.05534233795378672083228815205, 8.491647959787441076703482826]`; per-observation stationarity residual `1.446872568465895298285598402e-57`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10017: fpareto, face-invtrgamma-1e+06

Data: `[0.44884298889882046, 0.5265325556028752, 0.5832385938080731, 0.6351484955176038, 0.687121800336192, 0.742231154335141, 0.8035614314914231, 0.8753262151501601, 0.9646574466260144, 1.0863232755735668, 1.2810189778164804, 1.7655930855749113]`. Holds: `{'shape1': 0.3, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10017` (run through the slot wrapper).
Returned: `{'shape1': '0.29999999999999999', 'shape2': '15.343409623415438', 'shape3': '0.15370661238241637', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-4.22298601378809035367112301173619925975799560546875`.
Independent logLik at that point: `-4.222986013788148752752929983` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.87e-08`.
Better feasible point: `[0.2999999999999999888977697537, 15.34340980822664336670224383, 0.1537066095145552721591022129, 1.008784292115731506811471263, 0.0]`; logLik `-4.222986013788147816167168937`.
Stationary local point: `[0.2999999999999999888977697537, 15.34340980822664336670224383, 0.1537066095145552721591022129, 1.008784292115731506811471263, 0.0]`; logLik `-4.222986013788147816167168937`. Verified coefficient relative error: `1.87e-08`.
Local information eigenvalue range: `[0.2858098449971802569374921253, 1.676875307760365884274683424]`; per-observation stationarity residual `2.388413909109402827831716625e-61`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10026: fpareto, face-lnorm-1e+06

Data: `[0.17698958392421094, 0.3165261874667128, 0.4438725877326036, 0.5778030449797549, 0.7271377620782987, 0.9006546037111516, 1.110303545753946, 1.3752552159328302, 1.73069354460577, 2.252898754366237, 3.159296259192437, 5.6500500076221565]`. Holds: `{'shape2': 1.9802932530684678, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10026` (run through the slot wrapper).
Returned: `{'shape1': '0.89821980074347096', 'shape2': '1.9802932530684678', 'shape3': '0.89821998420080529', 'scale': '0.99999983117460689', 'min': '0'}`, status `0`, logLik `-16.72660327873882124549709260463714599609375`.
Independent logLik at that point: `-16.72660327873889673270719516` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.69e-07`.
Better feasible point: `[0.898219898775116802011219514, 1.980293253068467818067688313, 0.8982198987751507311577958288, 0.9999999999999830073954193278, 0.0]`; logLik `-16.72660327873888582491267019`.
Stationary local point: `[0.898219898775116802011219514, 1.980293253068467818067688313, 0.8982198987751507311577958288, 0.9999999999999830073954193278, 0.0]`; logLik `-16.72660327873888582491267019`. Verified coefficient relative error: `1.69e-07`.
Local information eigenvalue range: `[0.03540958617352398260858456616, 2.596265717959378233493858349]`; per-observation stationarity residual `1.396896269020657230519959356e-53`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10027: fpareto, face-lnorm-1e+06

Data: `[0.17698958392421094, 0.3165261874667128, 0.4438725877326036, 0.5778030449797549, 0.7271377620782987, 0.9006546037111516, 1.110303545753946, 1.3752552159328302, 1.73069354460577, 2.252898754366237, 3.159296259192437, 5.6500500076221565]`. Holds: `{'shape1': 0.3, 'shape2': 1.9802932530684678, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10027` (run through the slot wrapper).
Returned: `{'shape1': '0.29999999999999999', 'shape2': '1.9802932530684678', 'shape3': '8.4968161803598647', 'scale': '0.092214971569380355', 'min': '0'}`, status `0`, logLik `-18.27298772741631438520926167257130146026611328125`.
Independent logLik at that point: `-18.27298772741633153468465924` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.82e-07`.
Better feasible point: `[0.2999999999999999888977697537, 1.980293253068467818067688313, 8.496821974632976642365409473, 0.09221493604782302322991186716, 0.0]`; logLik `-18.2729877274163285109390373`.
Stationary local point: `[0.2999999999999999888977697537, 1.980293253068467818067688313, 8.496821974632976642365409473, 0.09221493604782302322991186716, 0.0]`; logLik `-18.2729877274163285109390373`. Verified coefficient relative error: `6.82e-07`.
Local information eigenvalue range: `[0.0008213965308441531860071986042, 1.340259367906570728212979578]`; per-observation stationarity residual `2.998754873665653365134197864e-44`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10034: fpareto, face-lnorm-1e+06

Data: `[0.17698958392421094, 0.3165261874667128, 0.4438725877326036, 0.5778030449797549, 0.7271377620782987, 0.9006546037111516, 1.110303545753946, 1.3752552159328302, 1.73069354460577, 2.252898754366237, 3.159296259192437, 5.6500500076221565]`. Holds: `{'shape2': 1.9802932530684678, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10034` (run through the slot wrapper).
Returned: `{'shape1': '0.90273219107424896', 'shape2': '1.9802932530684678', 'shape3': '0.89375005229905768', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `-16.726630399355332912136873346753418445587158203125`.
Independent logLik at that point: `-16.72663039935537807104898971` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.89e-08`.
Better feasible point: `[0.9027321739790369811157505189, 1.980293253068467818067688313, 0.8937500461519132065738246214, 1.008784292115731506811471263, 0.0]`; logLik `-16.72663039935537666942657066`.
Stationary local point: `[0.9027321739790369811157505189, 1.980293253068467818067688313, 0.8937500461519132065738246214, 1.008784292115731506811471263, 0.0]`; logLik `-16.72663039935537666942657066`. Verified coefficient relative error: `1.89e-08`.
Local information eigenvalue range: `[0.363513924781885366022748301, 1.555708448449602103388292988]`; per-observation stationarity residual `2.94134180376970397762438518e-62`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10084: fpareto, face-double-pareto-1e+06

Data: `[0.9992918544349726, 0.9992918544349726, 0.9992918544349726, 0.9992918544349726, 0.9992918544349726, 0.9992918544349726, 0.9992918544349726, 1.0007086473905342, 1.0007086473905342, 1.0007086473905342, 1.0007086473905342, 1.0007086473905342]`. Holds: `{'shape3': 0.4, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10084` (run through the slot wrapper).
Returned: `{'shape1': '70013325209.509811', 'shape2': '3114.0895231658474', 'shape3': '0.40000000000000002', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `69.5351538488672531457268632948398590087890625`.
Independent logLik at that point: `69.53515384886679775009906053` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.34e-06`.
Better feasible point: `[70013021034.07563009733947096, 3114.088986621133679818139905, 0.4000000000000000222044604925, 1.008784292115731506811471263, 0.0]`; logLik `69.53515384886699953720902361`.
Stationary local point: `[70013021034.07563009733947096, 3114.088986621133679818139905, 0.4000000000000000222044604925, 1.008784292115731506811471263, 0.0]`; logLik `69.53515384886699953720902361`. Verified coefficient relative error: `4.34e-06`.
Local information eigenvalue range: `[0.001778995030016435586939332573, 253.5961773358005482618237916]`; per-observation stationarity residual `5.40925896975248952085878059e-69`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10140: zmnbinom, face-Poisson-1e+06

Data: `[1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5]`. Holds: `{}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10140` (run through the slot wrapper).
Returned: `{'size': '5.7353723218502228', 'prob': '0.77511388544820692', 'p0': '0'}`, status `0`, logLik `-17.684784790596353332148282788693904876708984375`.
Independent logLik at that point: `-17.68478479059635674486905341` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.45e-08`.
Better feasible point: `[5.735371951688734394127874977, 0.775113875151507350519748035, 0.0]`; logLik `-17.68478479059635652783915121`.
Stationary local point: `[5.735371951688734394127874977, 0.775113875151507350519748035, 0.0]`; logLik `-17.68478479059635652783915121`. Verified coefficient relative error: `6.45e-08`.
Local information eigenvalue range: `[0.00833071180046703457000975443, 21.12848144583860905755741716]`; per-observation stationarity residual `3.661000027386657288322926301e-53`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10142: zmnbinom, face-Poisson-1e+06

Data: `[1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5]`. Holds: `{'prob': 0.41499877689089204}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10142` (run through the slot wrapper).
Returned: `{'size': '0.84558425364009249', 'prob': '0.41499877689089204', 'p0': '0'}`, status `0`, logLik `-18.0198270550488786057030665688216686248779296875`.
Independent logLik at that point: `-18.0198270550488817270210382` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.72e-08`.
Better feasible point: `[0.8455842137641897990602370739, 0.4149987768908920404697937556, 0.0]`; logLik `-18.01982705504887896069593627`.
Stationary local point: `[0.8455842137641897990602370739, 0.4149987768908920404697937556, 0.0]`; logLik `-18.01982705504887896069593627`. Verified coefficient relative error: `4.72e-08`.
Local information eigenvalue range: `[0.2073216663047825151454130637, 0.2073216663047825151454130637]`; per-observation stationarity residual `2.132094989891671353358473829e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10144: zmnbinom, face-Poisson-1e+06

Data: `[1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5]`. Holds: `{'p0': 0.314}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10144` (run through the slot wrapper).
Returned: `{'size': '5.7353723218502228', 'prob': '0.77511388544820692', 'p0': '0.314'}`, status `0`, logLik `-22.20731660567137311090846196748316287994384765625`.
Independent logLik at that point: `-22.20731660567137869602223467` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.45e-08`.
Better feasible point: `[5.735371951688734394127874977, 0.775113875151507350519748035, 0.3140000000000000013322676296]`; logLik `-22.20731660567137847899233246`.
Stationary local point: `[5.735371951688734394127874977, 0.775113875151507350519748035, 0.3140000000000000013322676296]`; logLik `-22.20731660567137847899233246`. Verified coefficient relative error: `6.45e-08`.
Local information eigenvalue range: `[0.00833071180046703457000975443, 21.12848144583860905755741716]`; per-observation stationarity residual `3.661000027386657288322926301e-53`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10146: zmnbinom, face-Poisson-1e+06

Data: `[1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5]`. Holds: `{'prob': 0.41499877689089204, 'p0': 0.314}`.
Call: `route_zmnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10146` (run through the slot wrapper).
Returned: `{'size': '0.84558425364009249', 'prob': '0.41499877689089204', 'p0': '0.314'}`, status `0`, logLik `-22.54235887012389838446324574761092662811279296875`.
Independent logLik at that point: `-22.54235887012390367817421946` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.72e-08`.
Better feasible point: `[0.8455842137641897990602370739, 0.4149987768908920404697937556, 0.3140000000000000013322676296]`; logLik `-22.54235887012390091184911753`.
Stationary local point: `[0.8455842137641897990602370739, 0.4149987768908920404697937556, 0.3140000000000000013322676296]`; logLik `-22.54235887012390091184911753`. Verified coefficient relative error: `4.72e-08`.
Local information eigenvalue range: `[0.2073216663047825151454130637, 0.2073216663047825151454130637]`; per-observation stationarity residual `2.132094989891671353358473829e-63`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/zmnbinom/mle.hpp:8`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10148: ztnbinom, face-Poisson-1e+06

Data: `[1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5]`. Holds: `{}`.
Call: `route_ztnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10148` (run through the slot wrapper).
Returned: `{'size': '5.7353723218502228', 'prob': '0.77511388544820692'}`, status `0`, logLik `-17.684784790596353332148282788693904876708984375`.
Independent logLik at that point: `-17.68478479059635674486905341` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.45e-08`.
Better feasible point: `[5.735371951688734394127874977, 0.775113875151507350519748035]`; logLik `-17.68478479059635652783915121`.
Stationary local point: `[5.735371951688734394127874977, 0.775113875151507350519748035]`; logLik `-17.68478479059635652783915121`. Verified coefficient relative error: `6.45e-08`.
Local information eigenvalue range: `[0.00833071180046703457000975443, 21.12848144583860905755741716]`; per-observation stationarity residual `3.661000027386657288322926301e-53`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ztnbinom/mle.hpp:34` (step stop): `if(maxit < std::ceil(std::log(std::min(tol,0.5))/std::log(0.6180339887498948482)))`. Execution-path causation remains open.

## Case 10150: ztnbinom, face-Poisson-1e+06

Data: `[1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 5]`. Holds: `{'prob': 0.4269728320855304}`.
Call: `route_ztnbinom(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10150` (run through the slot wrapper).
Returned: `{'size': '0.90873813000253367', 'prob': '0.42697283208553038'}`, status `0`, logLik `-17.994615690098225257997910375706851482391357421875`.
Independent logLik at that point: `-17.99461569009822897226511966` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.5e-08`.
Better feasible point: `[0.9087380891095489083401484499, 0.4269728320855303849157280638]`; logLik `-17.99461569009822623847061457`.
Stationary local point: `[0.9087380891095489083401484499, 0.4269728320855303849157280638]`; logLik `-17.99461569009822623847061457`. Verified coefficient relative error: `4.5e-08`.
Local information eigenvalue range: `[0.2250062082767425995510851612, 0.2250062082767425995510851612]`; per-observation stationarity residual `2.109724269844952828665389806e-63`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/ztnbinom/mle.hpp:34` (step stop): `if(maxit < std::ceil(std::log(std::min(tol,0.5))/std::log(0.6180339887498948482)))`. Execution-path causation remains open.

## Case 10171: emg, face-normal-1e+06

Data: `[-1.7316643535626306, -1.1503492468446153, -0.8122175678850617, -0.5485219378576116, -0.3186388939607459, -0.10463284250960252, 0.10463423577263281, 0.3186403447936282, 0.5485235148417794, 0.8122193701158308, 1.1503514598175495, 1.7316675741760756]`. Holds: `{'mu': 0.17576157248036894, 'lambda': 1.209185930245896}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10171` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '1.1680438762089727', 'lambda': '1.209185930245896'}`, status `0`, logLik `-20.5103893362391005439349100925028324127197265625`.
Independent logLik at that point: `-20.51038933623910554651356108` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.2e-08`.
Better feasible point: `[0.1757615724803689438715537108, 1.16804389026115301822607034, 1.20918593024589604567609058]`; logLik `-20.51038933623910429503735888`.
Stationary local point: `[0.1757615724803689438715537108, 1.16804389026115301822607034, 1.20918593024589604567609058]`; logLik `-20.51038933623910429503735888`. Verified coefficient relative error: `1.2e-08`.
Local information eigenvalue range: `[1.441126885136189135286044239, 1.441126885136189135286044239]`; per-observation stationarity residual `1.534703592374931259948511863e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 10175: emg, face-translated-exponential-1e+06

Data: `[0.042557882754399805, 0.13353024227514226, 0.2336140389637037, 0.3448399377694469, 0.4700033106063716, 0.6131043682529532, 0.7801586621830305, 0.9808295716510902, 1.2321442298149152, 1.5686167301316465, 2.079442692029216, 3.178055562012343]`. Holds: `{'mu': 0.17576157248036894}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10175` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.18773948063677129', 'lambda': '1.2272838552664691'}`, status `0`, logLik `-12.85519701553597116117089171893894672393798828125`.
Independent logLik at that point: `-12.85519701553597420544781338` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.47e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.1877394759902965500594487107, 1.227283856343641398840544061]`; logLik `-12.85519701553597341306823745`.
Stationary local point: `[0.1757615724803689438715537108, 0.1877394759902965500594487107, 1.227283856343641398840544061]`; logLik `-12.85519701553597341306823745`. Verified coefficient relative error: `2.47e-08`.
Local information eigenvalue range: `[0.2139868646839353043571887128, 0.9601590809004168506095425596]`; per-observation stationarity residual `1.881033317288046493628352801e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 10177: emg, face-translated-exponential-1e+06

Data: `[0.042557882754399805, 0.13353024227514226, 0.2336140389637037, 0.3448399377694469, 0.4700033106063716, 0.6131043682529532, 0.7801586621830305, 0.9808295716510902, 1.2321442298149152, 1.5686167301316465, 2.079442692029216, 3.178055562012343]`. Holds: `{'mu': 0.17576157248036894, 'sigma': 1.0518147225573364}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10177` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '1.0518147225573364', 'lambda': '1.5821368607133064'}`, status `0`, logLik `-16.527117888688348301684527541510760784149169921875`.
Independent logLik at that point: `-16.52711788868835018260216789` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.56e-08`.
Better feasible point: `[0.1757615724803689438715537108, 1.051814722557336434505259604, 1.582136820233281983054577389]`; logLik `-16.52711788868834888828856573`.
Stationary local point: `[0.1757615724803689438715537108, 1.051814722557336434505259604, 1.582136820233281983054577389]`; logLik `-16.52711788868834888828856573`. Verified coefficient relative error: `2.56e-08`.
Local information eigenvalue range: `[0.32952991542165952469857442, 0.32952991542165952469857442]`; per-observation stationarity residual `1.147871448350761743659182099e-59`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 10265: emg, constant-extra-1-0.0001

Data: `[0.9998, 0.9999, 1.0, 1.0001, 1.0003]`. Holds: `{'mu': 0.17576157248036894}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10265` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.38728973806434419', 'lambda': '1.556941820585114'}`, status `0`, logLik `-3.62253972790590950125988456420600414276123046875`.
Independent logLik at that point: `-3.62253972790592121295929957` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.69e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.3872897445909346234249717938, 1.556941846471669938017220123]`; logLik `-3.622539727905920605760209366`.
Stationary local point: `[0.1757615724803689438715537108, 0.3872897445909346234249717938, 1.556941846471669938017220123]`; logLik `-3.622539727905920605760209366`. Verified coefficient relative error: `1.69e-08`.
Local information eigenvalue range: `[0.4333549119273506038686485355, 0.9999998563150343169905258347]`; per-observation stationarity residual `9.331195668384280828596368405e-62`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 10266: emg, constant-extra-1-1e-08

Data: `[0.99999998, 0.99999999, 1.0, 1.00000001, 1.00000003]`. Holds: `{'mu': 0.17576157248036894}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10266` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.38728036566783652', 'lambda': '1.5569797016121618'}`, status `0`, logLik `-3.6224183084808174726276774890720844268798828125`.
Independent logLik at that point: `-3.622418308480826459845750188` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.01e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.3872803695833356780918842878, 1.556979689448399166874528722]`; logLik `-3.622418308480826055458688981`.
Stationary local point: `[0.1757615724803689438715537108, 0.3872803695833356780918842878, 1.556979689448399166874528722]`; logLik `-3.622418308480826055458688981`. Verified coefficient relative error: `1.01e-08`.
Local information eigenvalue range: `[0.433355010954396733363095517, 0.9999999977021642384115697575]`; per-observation stationarity residual `1.204277234435618554981930913e-66`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 10271: emg, constant-extra-1-0.0001

Data: `[0.9998, 0.9999, 1.0, 1.0001, 1.0003]`. Holds: `{'mu': 0.17576157248036894, 'sigma': 1.0518147225573364}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10271` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '1.0518147225573364', 'lambda': '2.1538513213860195'}`, status `0`, logLik `-5.56367052578992371536514838226139545440673828125`.
Independent logLik at that point: `-5.563670525789931224858647588` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.79e-08`.
Better feasible point: `[0.1757615724803689438715537108, 1.051814722557336434505259604, 2.153851359966672394120837099]`; logLik `-5.563670525789931044642999494`.
Stationary local point: `[0.1757615724803689438715537108, 1.051814722557336434505259604, 2.153851359966672394120837099]`; logLik `-5.563670525789931044642999494`. Verified coefficient relative error: `1.79e-08`.
Local information eigenvalue range: `[0.2246698392633226901383171191, 0.2246698392633226901383171191]`; per-observation stationarity residual `7.452580479850756491618222268e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 10272: emg, constant-extra-1-1e-08

Data: `[0.99999998, 0.99999999, 1.0, 1.00000001, 1.00000003]`. Holds: `{'mu': 0.17576157248036894, 'sigma': 1.0518147225573364}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10272` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '1.0518147225573364', 'lambda': '2.1539136945730286'}`, status `0`, logLik `-5.56363793443642862257547676563262939453125`.
Independent logLik at that point: `-5.56363793443642266294517849` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.34e-08`.
Better feasible point: `[0.1757615724803689438715537108, 1.051814722557336434505259604, 2.153913766581839971435959154]`; logLik `-5.563637934436422035200059574`.
Stationary local point: `[0.1757615724803689438715537108, 1.051814722557336434505259604, 2.153913766581839971435959154]`; logLik `-5.563637934436422035200059574`. Verified coefficient relative error: `3.34e-08`.
Local information eigenvalue range: `[0.2246614846902785346965210817, 0.2246614846902785346965210817]`; per-observation stationarity residual `1.097352822023377133951909027e-58`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 10273: emg, constant-extra-1-1e-12

Data: `[0.999999999998, 0.999999999999, 1.0, 1.000000000001, 1.000000000003]`. Holds: `{'mu': 0.17576157248036894, 'sigma': 1.0518147225573364}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10273` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '1.0518147225573364', 'lambda': '2.1539139933257503'}`, status `0`, logLik `-5.56363793118299554407713003456592559814453125`.
Independent logLik at that point: `-5.563637931183000076287597374` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.02e-07`.
Better feasible point: `[0.1757615724803689438715537108, 1.051814722557336434505259604, 2.153913772820204896871602559]`; logLik `-5.563637931182994189870954341`.
Stationary local point: `[0.1757615724803689438715537108, 1.051814722557336434505259604, 2.153913772820204896871602559]`; logLik `-5.563637931182994189870954341`. Verified coefficient relative error: `1.02e-07`.
Local information eigenvalue range: `[0.2246615448765245800415803841, 0.2246615448765245800415803841]`; per-observation stationarity residual `8.48433299774491920761569709e-55`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 10277: emg, constant-extra-1-0.0001

Data: `[0.9998, 0.9999, 1.0, 1.0001, 1.0003]`. Holds: `{'mu': 0.17576157248036894, 'lambda': 1.209185930245896}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10277` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.35202467300460843', 'lambda': '1.209185930245896'}`, status `0`, logLik `-3.721123661668940485469647683203220367431640625`.
Independent logLik at that point: `-3.721123661668949239276888164` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.83e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.3520246794421700895386526034, 1.20918593024589604567609058]`; logLik `-3.721123661668948816607339188`.
Stationary local point: `[0.1757615724803689438715537108, 0.3520246794421700895386526034, 1.20918593024589604567609058]`; logLik `-3.721123661668948816607339188`. Verified coefficient relative error: `1.83e-08`.
Local information eigenvalue range: `[0.5055502819607096892353750121, 0.5055502819607096892353750121]`; per-observation stationarity residual `1.586706689314058452118068039e-69`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 10278: emg, constant-extra-1-1e-08

Data: `[0.99999998, 0.99999999, 1.0, 1.00000001, 1.00000003]`. Holds: `{'mu': 0.17576157248036894, 'lambda': 1.209185930245896}`.
Call: `route_emg(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10278` (run through the slot wrapper).
Returned: `{'mu': '0.17576157248036894', 'sigma': '0.35201311104486116', 'lambda': '1.209185930245896'}`, status `0`, logLik `-3.721021276154345969189307652413845062255859375`.
Independent logLik at that point: `-3.721021276154355237060812364` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.95e-08`.
Better feasible point: `[0.1757615724803689438715537108, 0.352013093610597166494752658, 1.20918593024589604567609058]`; logLik `-3.72102127615435213693946131`.
Stationary local point: `[0.1757615724803689438715537108, 0.352013093610597166494752658, 1.20918593024589604567609058]`; logLik `-3.72102127615435213693946131`. Verified coefficient relative error: `4.95e-08`.
Local information eigenvalue range: `[0.5055325535564498119549107899, 0.5055325535564498119549107899]`; per-observation stationarity residual `4.609754998973688634797640126e-66`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/emg/logconcave_profile.hpp:42` (step stop): `if(std::abs(gu)<=64*std::numeric_limits<double>::epsilon()*su && std::abs(gb*b)<=64*std::numeric_limits<double>::epsilon()*sb) return {static_cast<double>(u),static_cast<double>(b),k,static_cast<double>(val),true};`. Execution-path causation remains open.

## Case 10289: foldnorm, constant-extra-1-0.0001

Data: `[0.9998, 0.9999, 1.0, 1.0001, 1.0003]`. Holds: `{'mu': 2.0248532542607856}`.
Call: `route_foldnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10289` (run through the slot wrapper).
Returned: `{'mu': '2.0248532542607856', 'sigma': '1.4020877974902539'}`, status `0`, logLik `-7.0203942901380624874718705541454255580902099609375`.
Independent logLik at that point: `-7.020394290138062901012225434` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.35e-08`.
Better feasible point: `[2.024853254260785551110757297, 1.402087750519700961244149598]`; logLik `-7.020394290138062064875876282`.
Stationary local point: `[2.024853254260785551110757297, 1.402087750519700961244149598]`; logLik `-7.020394290138062064875876282`. Verified coefficient relative error: `3.35e-08`.
Local information eigenvalue range: `[0.2980136594088575769152156765, 0.2980136594088575769152156765]`; per-observation stationarity residual `3.273873971038881787483723128e-66`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/foldnorm/mle.hpp:28` (step stop): `for (;k<maxit && b-a>tol;++k) {`. Execution-path causation remains open.

## Case 10290: foldnorm, constant-extra-1-1e-08

Data: `[0.99999998, 0.99999999, 1.0, 1.00000001, 1.00000003]`. Holds: `{'mu': 2.0248532542607856}`.
Call: `route_foldnorm(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10290` (run through the slot wrapper).
Returned: `{'mu': '2.0248532542607856', 'sigma': '1.4022220329517354'}`, status `0`, logLik `-7.020423119830315528133724001236259937286376953125`.
Independent logLik at that point: `-7.020423119830316864928944517` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.91e-08`.
Better feasible point: `[2.024853254260785551110757297, 1.402222006189897756284481379]`; logLik `-7.020423119830316593418066452`.
Stationary local point: `[2.024853254260785551110757297, 1.402222006189897756284481379]`; logLik `-7.020423119830316593418066452`. Verified coefficient relative error: `1.91e-08`.
Local information eigenvalue range: `[0.2981596956048304691130535777, 0.2981596956048304691130535777]`; per-observation stationarity residual `3.415113558060819841168524415e-68`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/foldnorm/mle.hpp:28` (step stop): `for (;k<maxit && b-a>tol;++k) {`. Execution-path causation remains open.

## Case 10334: fpareto, constant-extra-1-0.0001

Data: `[0.9998, 0.9999, 1.0, 1.0001, 1.0003]`. Holds: `{'shape3': 0.4, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10334` (run through the slot wrapper).
Returned: `{'shape1': '2.42842067679854e+38', 'shape2': '10403.339462473799', 'shape3': '0.40000000000000002', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `35.4788375513756903956164023838937282562255859375`.
Independent logLik at that point: `35.47883755137540432843848983` (118 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.12e-08`.
Better feasible point: `[2.428420752468424816356602675e+38, 10403.33946614425220199144901, 0.4000000000000000222044604925, 1.008784292115731506811471263, 0.0]`; logLik `35.4788375513754043288739929`.
Stationary local point: `[2.428420752468424816356602675e+38, 10403.33946614425220199144901, 0.4000000000000000222044604925, 1.008784292115731506811471263, 0.0]`; logLik `35.4788375513754043288739929`. Verified coefficient relative error: `3.12e-08`.
Local information eigenvalue range: `[0.0001793895830393164658853665082, 3119.0725600304053152993071]`; per-observation stationarity residual `4.465420098109468199848285846e-53`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10349: frechet, constant-extra-1-0.0001

Data: `[0.9998, 0.9999, 1.0, 1.0001, 1.0003]`. Holds: `{'lambda': 1.780221601423486}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10349` (run through the slot wrapper).
Returned: `{'lambda': '1.780221601423486', 'mu': '0.9996715977664874', 'sigma': '0.00023361169970872786'}`, status `0`, logLik `36.03567962322591711199493147432804107666015625`.
Independent logLik at that point: `36.03567962322591145456876063` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.02e-08`.
Better feasible point: `[1.780221601423485955351111443, 0.9996715977647882629498980101, 0.000233611702081304127975369536]`; logLik `36.03567962322591170037353885`.
Stationary local point: `[1.780221601423485955351111443, 0.9996715977647882629498980101, 0.000233611702081304127975369536]`; logLik `36.03567962322591170037353885`. Verified coefficient relative error: `1.02e-08`.
Local information eigenvalue range: `[0.686505077252796539650845266, 39.30426544438165078451427468]`; per-observation stationarity residual `1.240202563006951279116561604e-61`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 10354: frechet, constant-extra-1-1e-12

Data: `[0.999999999998, 0.999999999999, 1.0, 1.000000000001, 1.000000000003]`. Holds: `{'mu': 0.06127683692504238}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10354` (run through the slot wrapper).
Returned: `{'lambda': '664300514981.23621', 'mu': '0.061276836925042377', 'sigma': '0.93872316307433656'}`, status `0`, logLik `128.52106937708055056646117009222507476806640625`.
Independent logLik at that point: `128.521064955237831783298224` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `3.44e-08`.
Stationary local point: `[664299785992.4962326028368319, 0.06127683692504237722431525981, 0.9387231630743366209328537227]`; logLik `128.5210649592062392173113347`. Verified coefficient relative error: `—`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 10359: frechet, constant-extra-1-1e-08

Data: `[0.99999998, 0.99999999, 1.0, 1.00000001, 1.00000003]`. Holds: `{'sigma': 0.9001111496659718}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10359` (run through the slot wrapper).
Returned: `{'lambda': '63693423.031362787', 'mu': '0.09988884412751331', 'sigma': '0.90011114966597183'}`, status `0`, logLik `82.46937844650398119483725167810916900634765625`.
Independent logLik at that point: `82.46937844686534817581483605` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.62e-05`.
Better feasible point: `[63697638.22135011385449659929, 0.09988884412364533973152988859, 0.9001111496659718325119570181]`; logLik `82.46937861520362132472216941`.
Stationary local point: `[63697638.22135011385449659929, 0.09988884412364533973152988859, 0.9001111496659718325119570181]`; logLik `82.46937861520362132472216941`. Verified coefficient relative error: `6.62e-05`.
Local information eigenvalue range: `[1.533638778585477008516433754, 12.71973766229880262532067994]`; per-observation stationarity residual `6.936301532686298462192036693e-60`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 10360: frechet, constant-extra-1-1e-12

Data: `[0.999999999998, 0.999999999999, 1.0, 1.000000000001, 1.000000000003]`. Holds: `{'sigma': 0.9001111496659718}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10360` (run through the slot wrapper).
Returned: `{'lambda': '504728298336.08191', 'mu': '0.099888850332028212', 'sigma': '0.90011114966597183'}`, status `0`, logLik `126.951340386992598041615565307438373565673828125`.
Independent logLik at that point: `126.9513473835991645117065301` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.000624`.
Better feasible point: `[504728298336.0819091796875, 0.09988885033207821173562024342, 0.9001111496659718325119570181]`; logLik `127.030594894928333024951291`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 10418: gb2, constant-extra-1-0.0001

Data: `[0.9998, 0.9999, 1.0, 1.0001, 1.0003]`. Holds: `{'shape3': 0.7116483765738807, 'scale': 1.0079365599415882}`.
Call: `route_gb2(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10418` (run through the slot wrapper).
Returned: `{'shape1': '9.7401929601118814e+23', 'shape2': '7144.0966220391938', 'shape3': '0.71164837657388069', 'scale': '1.0079365599415882'}`, status `0`, logLik `35.6733813787489140167963341809809207916259765625`.
Independent logLik at that point: `35.67338137874897210054267146` (103 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.06e-06`.
Better feasible point: `[974023247911719160539538.1443, 7144.09714933775427046516874, 0.7116483765738806921419268292, 1.007936559941588150479674368]`; logLik `35.67338137874899477167567249`.
Stationary local point: `[974023247911719160539538.1443, 7144.09714933775427046516874, 0.7116483765738806921419268292, 1.007936559941588150479674368]`; logLik `35.67338137874899477167567249`. Verified coefficient relative error: `4.06e-06`.
Local information eigenvalue range: `[0.0005506946394778640708843216138, 2149.46035461528975062702962]`; per-observation stationarity residual `7.733518673738923895047166617e-73`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gb2/mle.hpp:83` (special difference): `double c=hb ? 0 : special::digamma(q)-special::digamma(p);`. Execution-path causation remains open.

## Case 10430: gumbel, constant-extra-1-0.0001

Data: `[0.9998, 0.9999, 1.0, 1.0001, 1.0003]`. Holds: `{}`.
Call: `route_gumbel(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10430` (run through the slot wrapper).
Returned: `{'mu': '0.99993789619195206', 'sigma': '0.00014131001844548807'}`, status `0`, logLik `36.41767675782720203869757824577391147613525390625`.
Independent logLik at that point: `36.41767675782720218161478748` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.19e-07`.
Better feasible point: `[0.9999378961730494347010838882, 0.000141309973329644389361651519]`; logLik `36.41767675782759929696604703`.
Stationary local point: `[0.9999378961730494347010838882, 0.000141309973329644389361651519]`; logLik `36.41767675782759929696604703`. Verified coefficient relative error: `3.19e-07`.
Local information eigenvalue range: `[1.533839123966946418415922724, 12.71976753924877764274447637]`; per-observation stationarity residual `4.394343545291662650063421672e-50`. This is a local identifiability check.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/gumbel/mle.hpp:96` (step stop): `if (std::abs(dth) < std::abs(theta) * tol) break;`. Execution-path causation remains open.

## Case 10483: invburr, constant-extra-1-1e-12

Data: `[0.999999999998, 0.999999999999, 1.0, 1.000000000001, 1.000000000003]`. Holds: `{'shape1': 0.855256299891619}`.
Call: `route_invburr(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10483` (run through the slot wrapper).
Returned: `{'shape1': '0.855256299891619', 'shape2': '1019638991699.7413', 'scale': '1.0000000000003357'}`, status `0`, logLik `128.118736661423241685042739845812320709228515625`.
Independent logLik at that point: `128.1187906503408656073966206` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `4.21e-07`.
Stationary local point: `[0.8552562998916189984655034095, 1019651840251.461683462597829, 1.000000000000335684409340019]`; logLik `128.1187906523585445943826126`. Verified coefficient relative error: `—`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/invburr/mle.hpp:152` (step stop): `double scale0 = std::numeric_limits<double>::quiet_NaN(), double reltol = 1e-12) {`. Execution-path causation remains open.

## Case 10510: invgamma, constant-extra-1-1e-12

Data: `[0.999999999998, 0.999999999999, 1.0, 1.000000000001, 1.000000000003]`. Holds: `{}`.
Call: `route_invgamma(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10510` (run through the slot wrapper).
Returned: `{'alpha': '3.3782845557972286e+23', 'beta': '3.3782845557979044e+23'}`, status `0`, logLik `128.3473703131043066605343483388423919677734375`.
Independent logLik at that point: `128.347415323234460688513165` (103 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `3.51e-07`.
Stationary local point: `[337834537970659655690885.1249, 337834537970727236106649.3403]`; logLik `128.3474153236420776169203593`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/invgamma/mle.hpp:49`: Reciprocals are rounded to double before the gamma fit (line 53); nearly constant transformed data lose likelihood information.

## Case 10725: fpareto, support-lower-1e-200

Data: `[1e-200, 2e-200, 3e-200, 5e-200, 9e-200]`. Holds: `{'shape2': 1.9802932530684678, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10725` (run through the slot wrapper).
Returned: `{'shape1': '1.6576416888402374', 'shape2': '1.9802932530684678', 'shape3': '0.99288729429257838', 'scale': '4.5049117984026707e-200', 'min': '0'}`, status `0`, logLik `2291.1399783436563666327856481075286865234375`.
Independent logLik at that point: `2291.139978343656046666997524` (280 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.38e-07`.
Better feasible point: `[1.657641900414666625829818286, 1.980293253068467818067688313, 0.9928872341194263628709242683, 4.504912420433219974573257227e-200, 0.0]`; logLik `2291.139978343656047431789735`.
Stationary local point: `[1.657641900414666625829818286, 1.980293253068467818067688313, 0.9928872341194263628709242683, 4.504912420433219974573257227e-200, 0.0]`; logLik `2291.139978343656047431789735`. Verified coefficient relative error: `1.38e-07`.
Local information eigenvalue range: `[0.007146068082723444902701428049, 3.503906953467902605332627343]`; per-observation stationarity residual `1.527083307479763717296505671e-51`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10746: fpareto, support-lower-1e-06

Data: `[1e-06, 2e-06, 3e-06, 4.9999999999999996e-06, 9e-06]`. Holds: `{'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10746` (run through the slot wrapper).
Returned: `{'shape1': '597.18829773046537', 'shape2': '0.19427242890797772', 'shape3': '51.06353191580785', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `57.803099473324181190037052147090435028076171875`.
Independent logLik at that point: `57.80309947332392941390293653` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `2.33e-07`.
Better feasible point: `[597.1883148742211625629582836, 0.1942724497294429767349542881, 51.06351999272670912719659564, 1.008784292115731506811471263, 0.0]`; logLik `57.80309947332392946440543225`.
Stationary local point: `[597.1883148742211625629582836, 0.1942724497294429767349542881, 51.06351999272670912719659564, 1.008784292115731506811471263, 0.0]`; logLik `57.80309947332392946440543225`. Verified coefficient relative error: `2.33e-07`.
Local information eigenvalue range: `[0.0003021565317793981678846174643, 378.264163713979183882242513]`; per-observation stationarity residual `8.912999161660590841289255412e-43`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10756: fpareto, support-lower-1e-17

Data: `[1e-17, 2e-17, 3e-17, 5.0000000000000005e-17, 9.000000000000001e-17]`. Holds: `{'shape2': 1.9802932530684678, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10756` (run through the slot wrapper).
Returned: `{'shape1': '1.3442111283897202e+32', 'shape2': '1.9802932530684678', 'shape3': '0.66027205650863752', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `184.260810229007148564051021821796894073486328125`.
Independent logLik at that point: `184.2608102290051018917958859` (112 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.87e-08`.
Better feasible point: `[1.344211153484441304212497823e+32, 1.980293253068467818067688313, 0.660272057218004352297550209, 1.008784292115731506811471263, 0.0]`; logLik `184.2608102290051024047933368`.
Stationary local point: `[1.344211153484441304212497823e+32, 1.980293253068467818067688313, 0.660272057218004352297550209, 1.008784292115731506811471263, 0.0]`; logLik `184.2608102290051024047933368`. Verified coefficient relative error: `1.87e-08`.
Local information eigenvalue range: `[0.2619915127932652572964999618, 1.754875335227935235283260837]`; per-observation stationarity residual `2.393809120453082861594046638e-62`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10760: fpareto, support-lower-1e-17

Data: `[1e-17, 2e-17, 3e-17, 5.0000000000000005e-17, 9.000000000000001e-17]`. Holds: `{'shape1': 0.3, 'shape2': 1.9802932530684678, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10760` (run through the slot wrapper).
Returned: `{'shape1': '0.29999999999999999', 'shape2': '1.9802932530684678', 'shape3': '0.012802157657872312', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `166.742727407645901394062093459069728851318359375`.
Independent logLik at that point: `166.7427274076458490105419423` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.29e-08`.
Better feasible point: `[0.2999999999999999888977697537, 1.980293253068467818067688313, 0.01280215749306472354134770643, 1.008784292115731506811471263, 0.0]`; logLik `166.7427274076458494241941452`.
Stationary local point: `[0.2999999999999999888977697537, 1.980293253068467818067688313, 0.01280215749306472354134770643, 1.008784292115731506811471263, 0.0]`; logLik `166.7427274076458494241941452`. Verified coefficient relative error: `1.29e-08`.
Local information eigenvalue range: `[0.9984062016377594405101269116, 0.9984062016377594405101269116]`; per-observation stationarity residual `7.612091960518246602455355983e-64`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10762: fpareto, support-lower-1e-06

Data: `[1e-06, 2e-06, 3e-06, 4.9999999999999996e-06, 9e-06]`. Holds: `{'shape3': 0.4, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10762` (run through the slot wrapper).
Returned: `{'shape1': '91558404749959.5', 'shape2': '2.7259163404346203', 'shape3': '0.40000000000000002', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `57.54255995893434061372317955829203128814697265625`.
Independent logLik at that point: `57.54255995893414664418031855` (70 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `3.76e-08`.
Better feasible point: `[91558408190829.43882666893359, 2.725916343612529397864053235, 0.4000000000000000222044604925, 1.008784292115731506811471263, 0.0]`; logLik `57.54255995893414664876051704`.
Stationary local point: `[91558408190829.43882666893359, 2.725916343612529397864053235, 0.4000000000000000222044604925, 1.008784292115731506811471263, 0.0]`; logLik `57.54255995893414664876051704`. Verified coefficient relative error: `3.76e-08`.
Local information eigenvalue range: `[0.001295944689176637909111460481, 414.7182390573725871617385681]`; per-observation stationarity residual `1.446506420855735981778371261e-54`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10763: fpareto, support-lower-1e-12

Data: `[1e-12, 2e-12, 3e-12, 5e-12, 9e-12]`. Holds: `{'shape3': 0.4, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10763` (run through the slot wrapper).
Returned: `{'shape1': '2.0758514585804846e+30', 'shape2': '2.7259163416638574', 'shape3': '0.40000000000000002', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `126.6201127487567106300048180855810642242431640625`.
Independent logLik at that point: `126.6201127487555186043860734` (110 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `4.99e-08`.
Better feasible point: `[2.075851562216813912824905903e+30, 2.725916343612510096609819022, 0.4000000000000000222044604925, 1.008784292115731506811471263, 0.0]`; logLik `126.6201127487555186061037704`.
Stationary local point: `[2.075851562216813912824905903e+30, 2.725916343612510096609819022, 0.4000000000000000222044604925, 1.008784292115731506811471263, 0.0]`; logLik `126.6201127487555186061037704`. Verified coefficient relative error: `4.99e-08`.
Local information eigenvalue range: `[0.0002756041641851514739792691078, 1950.086234061534633926405024]`; per-observation stationarity residual `6.290883559988739361213001466e-52`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10764: fpareto, support-lower-1e-17

Data: `[1e-17, 2e-17, 3e-17, 5.0000000000000005e-17, 9.000000000000001e-17]`. Holds: `{'shape3': 0.4, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10764` (run through the slot wrapper).
Returned: `{'shape1': '8.8466269216486622e+43', 'shape2': '2.7259163249692513', 'shape3': '0.40000000000000002', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `184.18474007360777022768161259591579437255859375`.
Independent logLik at that point: `184.1847400736066598845888128` (123 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `6.92e-07`.
Better feasible point: `[8.846633045162973791159507207e+43, 2.725916343612509982135003406, 0.4000000000000000222044604925, 1.008784292115731506811471263, 0.0]`; logLik `184.184740073606660041761916`.
Stationary local point: `[8.846633045162973791159507207e+43, 2.725916343612509982135003406, 0.4000000000000000222044604925, 1.008784292115731506811471263, 0.0]`; logLik `184.184740073606660041761916`. Verified coefficient relative error: `6.92e-07`.
Local information eigenvalue range: `[0.0001312047202081390352832662099, 4096.279377929726004404217957]`; per-observation stationarity residual `5.591324813803704815955511924e-42`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10772: fpareto, support-lower-1e-17

Data: `[1e-17, 2e-17, 3e-17, 5.0000000000000005e-17, 9.000000000000001e-17]`. Holds: `{'shape2': 1.9802932530684678, 'shape3': 0.4, 'scale': 1.0087842921157315, 'min': 0.0}`.
Call: `route_fpareto(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10772` (run through the slot wrapper).
Returned: `{'shape1': '8.143375877108428e+31', 'shape2': '1.9802932530684678', 'shape3': '0.40000000000000002', 'scale': '1.0087842921157315', 'min': '0'}`, status `0`, logLik `183.875813606741900230190367437899112701416015625`.
Independent logLik at that point: `183.875813606740597365825995` (111 digits).
Evidence: stationary local refinement: coefficient relative error exceeds 1e-8. Worst relative discrepancy: `1.99e-07`.
Better feasible point: `[8.143377498954910003333241608e+31, 1.980293253068467818067688313, 0.4000000000000000222044604925, 1.008784292115731506811471263, 0.0]`; logLik `183.8758136067406370310954205`.
Stationary local point: `[8.143377498954910003333241608e+31, 1.980293253068467818067688313, 0.4000000000000000222044604925, 1.008784292115731506811471263, 0.0]`; logLik `183.8758136067406370310954205`. Verified coefficient relative error: `1.99e-07`.
Local information eigenvalue range: `[0.3999998406708940626647595648, 0.3999998406708940626647595648]`; per-observation stationarity residual `9.901506677989726687159270567e-55`. This is a local identifiability check.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/fpareto/mle.hpp:9`: inspect the executed estimator/likelihood path; a more specific causal line is not established by this witness.

## Case 10789: nsbeta, support-lower-1e-12

Data: `[1e-12, 2e-12, 3e-12, 5e-12, 9e-12]`. Holds: `{'shape1': 2.0063421787377256, 'min': 0.0, 'max': 1.0}`.
Call: `route_nsbeta(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10789` (run through the slot wrapper).
Returned: `{'shape1': '2.0063421787377256', 'shape2': '501721367735.86334', 'min': '0', 'max': '1'}`, status `0`, logLik `126.8220970942260237279697321355342864990234375`.
Independent logLik at that point: `126.8221598307618378960633616` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `4.95e-07`.
Stationary local point: `[2.006342178737725578940853666, 501585544682.4234821650753439, 0.0, 1.0]`; logLik `126.8221601984871922462244891`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/nsbeta/mle.hpp:24`: The held-shape score subtracts nearby digammas; line 25 also differences lgammas. The helper call omits maxit and uses its default inner budget. This bypasses the repaired held-shape beta implementation.

## Case 10791: nsbeta, support-lower-1e-17

Data: `[1e-17, 2e-17, 3e-17, 5.0000000000000005e-17, 9.000000000000001e-17]`. Holds: `{'shape1': 2.0063421787377256, 'min': 0.0, 'max': 1.0}`.
Call: `route_nsbeta(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10791` (run through the slot wrapper).
Returned: `{'shape1': '2.0063421787377256', 'shape2': '427914928881440.94', 'min': '0', 'max': '1'}`, status `0`, logLik `148.587322732861167651208234019577503204345703125`.
Independent logLik at that point: `146.5416446517512395618003336` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.255`.
Better feasible point: `[2.006342178737725578940853666, 50158554468443104.69940247494, 0.0, 1.0]`; logLik `184.3867875233383654124343134`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/nsbeta/mle.hpp:24`: The held-shape score subtracts nearby digammas; line 25 also differences lgammas. The helper call omits maxit and uses its default inner budget. This bypasses the repaired held-shape beta implementation.

## Case 10792: nsbeta, support-lower-1e-200

Data: `[1e-200, 2e-200, 3e-200, 5e-200, 9e-200]`. Holds: `{'shape1': 2.0063421787377256, 'min': 0.0, 'max': 1.0}`.
Call: `route_nsbeta(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10792` (run through the slot wrapper).
Returned: `{'shape1': '2.0063421787377256', 'shape2': '427914928881440.94', 'min': '0', 'max': '1'}`, status `0`, logLik `-1971.5545710609248999389819800853729248046875`.
Independent logLik at that point: `-1973.600249142034774683415708` (280 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.0423`.
Better feasible point: `[2.006342178737725578940853666, 1752739548698384140.485610965, 0.0, 1.0]`; logLik `-1890.158823676201634479804139`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/nsbeta/mle.hpp:24`: The held-shape score subtracts nearby digammas; line 25 also differences lgammas. The helper call omits maxit and uses its default inner budget. This bypasses the repaired held-shape beta implementation.

## Case 10799: nsbeta, support-upper-1e-12

Data: `[0.999999999999, 0.999999999998, 0.999999999997, 0.999999999995, 0.999999999991]`. Holds: `{'shape2': 3.0460502693524503, 'min': 0.0, 'max': 1.0}`.
Call: `route_nsbeta(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10799` (run through the slot wrapper).
Returned: `{'shape1': '761965856884.37634', 'shape2': '3.0460502693524503', 'min': '0', 'max': '1'}`, status `0`, logLik `126.54238661559202228090725839138031005859375`.
Independent logLik at that point: `126.5496980640750909973022056` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `5.78e-05`.
Better feasible point: `[761512504326.9778670247387344, 3.046050269352450268200982464, 0.0, 1.0]`; logLik `126.5497007619562388575133575`.
Stationary local point: `[761512504326.9778670247387344, 3.046050269352450268200982464, 0.0, 1.0]`; logLik `126.5497007619562388575133575`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/nsbeta/mle.hpp:24`: The held-shape score subtracts nearby digammas; line 25 also differences lgammas. The helper call omits maxit and uses its default inner budget. This bypasses the repaired held-shape beta implementation.

## Case 10927: frechet, support-lower-ulp

Data: `[0.061276836925042384, 0.06127683692504239, 0.0612768369250424, 0.06127683692504241, 0.06127683692504244]`. Holds: `{}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10927` (run through the slot wrapper).
Returned: `{'lambda': '2.2842236669478853', 'mu': '0.061276836925042363', 'sigma': '2.52914742600953e-17'}`, status `0`, logLik `186.118414973321961269903113134205341339111328125`.
Independent logLik at that point: `185.5755426132204215593969155` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.00292`.
Better feasible point: `[354.8741874794966660293019334, 0.06127683692503739069356515584, 5.005401267254987536624754327e-15]`; logLik `185.8569803081737982072560887`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 10928: frechet, support-lower-ulp

Data: `[0.061276836925042384, 0.06127683692504239, 0.0612768369250424, 0.06127683692504241, 0.06127683692504244]`. Holds: `{'lambda': 1.780221601423486}`.
Call: `route_frechet(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10928` (run through the slot wrapper).
Returned: `{'lambda': '1.780221601423486', 'mu': '0.06127683692504237', 'sigma': '1.9214656564031806e-17'}`, status `0`, logLik `185.7522324252987573345308192074298858642578125`.
Independent logLik at that point: `185.7522324252987734548752156` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.0019`.
Better feasible point: `[1.780221601423485955351111443, 0.06127683692504237339141403261, 1.921465685883944035557126023e-17]`; logLik `186.1042753932457944886878911`.
Stationary local point: `[1.780221601423485955351111443, 0.06127683692504237339141403261, 1.921465685883944035557126023e-17]`; logLik `186.1042753932457944886878911`. Verified coefficient relative error: `—`.
Suspected cause / path: Candidate seam `univariateML-cpp/include/cppunivariate/distributions/frechet/mle.hpp:12` (step stop): `if(x.size()<3 \|\| !(tol>0) \|\| !std::isfinite(tol) \|\| maxit<1) return bad;`. Execution-path causation remains open.

## Case 10945: gev, support-lower-ulp

Data: `[-3.221210133483815, -3.2212101334838077, -3.2212101334838006, -3.2212101334837864, -3.221210133483758]`. Holds: `{}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10945` (run through the slot wrapper).
Returned: `{'mu': '-3.2212101334838055', 'sigma': '1.133797447947754e-14', 'xi': '0.43778550009748662'}`, status `0`, logLik `151.461055945324716276445542462170124053955078125`.
Independent logLik at that point: `151.460772652953639469097293` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `1.87e-06`.
Better feasible point: `[-3.221210133483805558687844129, 1.133797448002882467378443884e-14, 0.4377855000244455830614165727]`; logLik `151.4610559453246915094780243`.
Stationary local point: `[-3.221210133483805558687844129, 1.133797448002882467378443884e-14, 0.4377855000244455830614165727]`; logLik `151.4610559453246915094780243`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 10949: gev, support-lower-1e-06

Data: `[-3.2212091334838218, -3.221208133483822, -3.221207133483822, -3.221205133483822, -3.221201133483822]`. Holds: `{'sigma': 0.9955800131732704}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10949` (run through the slot wrapper).
Returned: `{'mu': '-4.2167811466570928', 'sigma': '0.99558001317327038', 'xi': '-1'}`, status `0`, logLik `0.022123808247911457`.
Independent logLik at that point: `-inf` (70 digits).
Evidence: returned coefficients exclude an observation from support. Worst relative discrepancy: `inf`.
Exact support exclusion: `{'bound': 'upper', 'observation_index': 4, 'exact_bound': '-7253500112221581/2251799813685248', 'exact_observation': '-1813375028055395/562949953421312', 'exact_gap': '1/2251799813685248', 'bound_expression': 'Piecewise((p_mu - p_sigma/p_xi, p_xi < 0), (oo, True))', 'basis': 'exact binary64 rational support comparison'}`. This is a returned-coefficient feasibility defect; a one-ULP projection can be practically small even though the exact returned-point likelihood is zero.
Suspected cause / path: Boundary reconstruction/projection leaves the exact returned support short of the data; inspect `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 10957: gev, support-lower-ulp

Data: `[-3.221210133483815, -3.2212101334838077, -3.2212101334838006, -3.2212101334837864, -3.221210133483758]`. Holds: `{'xi': 0.30843601546572924}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10957` (run through the slot wrapper).
Returned: `{'mu': '-3.221210133483805', 'sigma': '1.1820064030739609e-14', 'xi': '0.30843601546572924'}`, status `0`, logLik `151.44206098990042619334417395293712615966796875`.
Independent logLik at that point: `151.4411913402561104867183839` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `5.74e-06`.
Better feasible point: `[-3.221210133483804845116276171, 1.182006403073960854713162974e-14, 0.3084360154657292407698321313]`; logLik `151.4420609899004094942920511`.
Stationary local point: `[-3.221210133483804845116276171, 1.182006403073960854713162974e-14, 0.3084360154657292407698321313]`; logLik `151.4420609899004094942920511`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 10965: gev, support-lower-1e-12

Data: `[-3.221210133482822, -3.2212101334818217, -3.221210133480822, -3.221210133478822, -3.221210133474822]`. Holds: `{'mu': 0.006623074223695902, 'sigma': 0.9955800131732704, 'xi': 0.30843601546572924}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10965` (run through the slot wrapper).
Returned: `{'mu': '0.0066230742236959017', 'sigma': '0.99558001317327038', 'xi': '0.30843601546572924'}`, status `0`, logLik `-41036369114802594289136740034572615417856.0`.
Independent logLik at that point: `-4.100552386676399200489490355e+40` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `0.000752`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 10966: gev, support-lower-ulp

Data: `[-3.221210133483815, -3.2212101334838077, -3.2212101334838006, -3.2212101334837864, -3.221210133483758]`. Holds: `{'mu': 0.006623074223695902, 'sigma': 0.9955800131732704, 'xi': 0.30843601546572924}`.
Call: `route_gev(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 10966` (run through the slot wrapper).
Returned: `{'mu': '0.0066230742236959017', 'sigma': '0.99558001317327038', 'xi': '0.30843601546572924'}`, status `0`, logLik `-426856303673594073816754690632074871866054934528.0`.
Independent logLik at that point: `-3.800279059746670700979161633e+47` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `0.11`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/gev/mle.hpp:263`: Affine restoration rounds mu and sigma independently. On the xi=-1 face the exact restored upper endpoint mu+sigma can fall below the largest observation. Recheck feasibility against the original data after restoration. Other witnesses require their own acceptance-path review.

## Case 11011: nsbeta, support-upper-ulp

Data: `[0.9999999999999999, 0.9999999999999998, 0.9999999999999997, 0.9999999999999994, 0.999999999999999]`. Holds: `{'shape2': 3.0460502693524503, 'min': 0.0, 'max': 1.0}`.
Call: `route_nsbeta(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 11011` (run through the slot wrapper).
Returned: `{'shape1': '708933651564869.5', 'shape2': '3.0460502693524503', 'min': '0', 'max': '1'}`, status `0`, logLik `150.37351075048132997835637070238590240478515625`.
Independent logLik at that point: `151.1686514684529226171302765` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `0.139`.
Better feasible point: `[6859095429003743.692437163309, 3.046050269352450268200982464, 0.0, 1.0]`; logLik `172.078653972959125947898573`.
Stationary local point: `[6859095429003743.692437163309, 3.046050269352450268200982464, 0.0, 1.0]`; logLik `172.078653972959125947898573`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/nsbeta/mle.hpp:24`: The held-shape score subtracts nearby digammas; line 25 also differences lgammas. The helper call omits maxit and uses its default inner budget. This bypasses the repaired held-shape beta implementation.

## Case 11117: triang, support-lower-ulp

Data: `[0.03627073533558047, 0.03627073533558048, 0.036270735335580485, 0.0362707353355805, 0.03627073533558053]`. Holds: `{}`.
Call: `route_triang(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 11117` (run through the slot wrapper).
Returned: `{'a': '0.036270735335580472', 'b': '0.036270735335580541', 'c': '0.036270735335580472'}`, status `0`, logLik `187.0510072650271240490837953984737396240234375`.
Independent logLik at that point: `187.0509892942417841961072884` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `9.61e-08`.
Better feasible point: `[0.03627073533558047152558501125, 0.03627073533558054100452920694, 0.03627073533558047152558501125]`; logLik `187.0510072650271454427372135`.
Stationary local point: `[0.03627073533558047152558501125, 0.03627073533558054100452920694, 0.03627073533558047152558501125]`; logLik `187.0510072650271454427372135`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp:229`: Affine endpoint restoration can put the returned upper bound one ULP below the data; the final gate checks coefficient ordering but not all original observations. Recheck original-data support before Ok.

## Case 11118: triang, support-upper-ulp

Data: `[1.0045256123727766, 1.0045256123727764, 1.0045256123727762, 1.0045256123727757, 1.0045256123727748]`. Holds: `{}`.
Call: `route_triang(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 11118` (run through the slot wrapper).
Returned: `{'a': '1.0045256123727744', 'b': '1.0045256123727766', 'c': '1.0045256123727766'}`, status `0`, logLik `169.7223277510285015523550100624561309814453125`.
Independent logLik at that point: `169.7223097802431514606764854` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `1.06e-07`.
Better feasible point: `[1.004525612372774376230586385, 1.004525612372776599556800647, 1.004525612372776599556800647]`; logLik `169.7223277510285127073064104`.
Stationary local point: `[1.004525612372774376230586385, 1.004525612372776599556800647, 1.004525612372776599556800647]`; logLik `169.7223277510285127073064104`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp:229`: Affine endpoint restoration can put the returned upper bound one ULP below the data; the final gate checks coefficient ordering but not all original observations. Recheck original-data support before Ok.

## Case 11119: triang, support-lower-ulp

Data: `[0.03627073533558047, 0.03627073533558048, 0.036270735335580485, 0.0362707353355805, 0.03627073533558053]`. Holds: `{'a': 0.036270735335580465}`.
Call: `route_triang(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 11119` (run through the slot wrapper).
Returned: `{'a': '0.036270735335580465', 'b': '0.036270735335580541', 'c': '0.036270735335580472'}`, status `0`, logLik `186.580081805241434267372824251651763916015625`.
Independent logLik at that point: `186.5744383952201598958875278` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `3.02e-05`.
Better feasible point: `[0.03627073533558046458669110734, 0.03627073533558054261207140974, 0.03627073533558047152558501125]`; logLik `186.5800818052414518332719691`.
Stationary local point: `[0.03627073533558046458669110734, 0.03627073533558054261207140974, 0.03627073533558047152558501125]`; logLik `186.5800818052414518332719691`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp:229`: Affine endpoint restoration can put the returned upper bound one ULP below the data; the final gate checks coefficient ordering but not all original observations. Recheck original-data support before Ok.

## Case 11122: triang, support-upper-ulp

Data: `[1.0045256123727766, 1.0045256123727764, 1.0045256123727762, 1.0045256123727757, 1.0045256123727748]`. Holds: `{'b': 1.0045256123727768}`.
Call: `route_triang(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 11122` (run through the slot wrapper).
Returned: `{'a': '1.0045256123727744', 'b': '1.0045256123727768', 'c': '1.0045256123727766'}`, status `0`, logLik `169.2514022912428117706440389156341552734375`.
Independent logLik at that point: `169.2457588812215271604567248` (70 digits).
Evidence: feasible higher-likelihood point (70 digits). Worst relative discrepancy: `3.33e-05`.
Better feasible point: `[1.004525612372774324789235896, 1.004525612372776821601405572, 1.004525612372776599556800647]`; logLik `169.2514022912428190978411661`.
Stationary local point: `[1.004525612372774324789235896, 1.004525612372776821601405572, 1.004525612372776599556800647]`; logLik `169.2514022912428190978411661`. Verified coefficient relative error: `—`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp:229`: Affine endpoint restoration can put the returned upper bound one ULP below the data; the final gate checks coefficient ordering but not all original observations. Recheck original-data support before Ok.

## Case 11130: triang, support-upper-ulp

Data: `[1.0045256123727766, 1.0045256123727764, 1.0045256123727762, 1.0045256123727757, 1.0045256123727748]`. Holds: `{'b': 1.0045256123727768, 'c': 0.3888433021027595}`.
Call: `route_triang(x, holds above)` with unchanged shared binding defaults. Replay: `python3 dev/research/evaluations/2026-11-07-fit-audit-data/reproduce.py 11130` (run through the slot wrapper).
Returned: `{'a': '0.38884330210275952', 'b': '1.0045256123727768', 'c': '0.38884330210275952'}`, status `0`, logLik `-165.733255917412435564983752556145191192626953125`.
Independent logLik at that point: `-166.3038672981450969549506765` (70 digits).
Evidence: logLik discrepancy. Worst relative discrepancy: `0.00344`.
Suspected cause / path: `univariateML-cpp/include/cppunivariate/distributions/triang/mle.hpp:229`: Affine endpoint restoration can put the returned upper bound one ULP below the data; the final gate checks coefficient ordering but not all original observations. Recheck original-data support before Ok.

