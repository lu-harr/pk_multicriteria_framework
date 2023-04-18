<h2> Demonstration: Zoonotic malaria surveillance site selection in northwest Indonesia </h2>

<h3> How to use the tool </h3>

- To select sites, navigate to the **Map** tab, select an objective and edit constraints in the toolbar at left. Press **Go!**
- Selected sites can be downloaded from the table at the bottom of the Map tab.
  - To select a subset of the sites suggested, *brush* over them in the map. The table will then show brushed sites only.
- Constraint surfaces are visualised in the **Constraints** tab.

<h3> Covariate layers </h3>

There are several data layers included in this tool from external sources:

- Shearer, F.M., Huang, Z., Weiss, D.J., Wiebe, A., Gibson, H.S., Battle, K.E., Pigott, D.M., Brady, O.J., Putaporntip, C., Jongwutiwes, S. and Lau, Y.L., et al. **Estimating geographical variation in the risk of zoonotic Plasmodium knowlesi infection in countries eliminating malaria**. PLoS neglected tropical diseases, 10(8), p.e0004915 (2016). https://doi.org/10.1371/journal.pntd.0004915
  - Predictions of relative risk of *Plasmodium knowlesi* malaria in humans, summarised into prediction mean and standard deviation surfaces.
- Weiss, D., Nelson, A., Gibson, H. et al. **A global map of travel time to cities to assess inequalities in accessibility in 2015**. *Nature* **553**, 333–336 (2018). http://doi.org/10.1038/nature25181
  - A predictive map showing the estimated time to travel from every point on earth to the nearest (in time) city.  Contains data from OpenStreetMap © OpenStreetMap contributors. Accessed through the malariaAtlas R package (https://doi.org/10.1186/s12936-018-2500-5). Transformed into surface of urban accessibility (see Supplementary Information of paper).
- Potapov, P., Hansen, M. C., Laestadius L., Turubanova S., Yaroshenko A., Thies C., Smith W., Zhuravleva I., Komarova A., Minnemeyer S., Esipova E. **The last frontiers of wilderness: Tracking loss of intact forest landscapes from 2000 to 2013**. Science Advances, 2017; 3:e1600821. https://doi.org/10.1126/sciadv.1600821
  - Intact Forest Landscapes surfaces of intact and disturbed forest (2013). Transformed into surface of total forest cover (see Supplementary Information of paper).
  
![image](workflow_with_grids.pdf)

<h3> TODO </h3>

- Rest of Borneo onto the maps ... rest of surrounding islands?
- Should explain percentages in constraint map ... it's not intuitive (add popover)