class MBP16Selector(AdditivePayoffSelector):
    """Selector to model boni behaviour described in MPB16ValueModel."""

    EPSILON = 1  # used for epsilon method

    def __init__(self, bidder_id, items, valuations, limits, non_additive_boni, hierarchy_flattened=None):
        """Adds boni to packages of band items, according to Bichler et. al 2016,
        package valuations based on Bachelor Thesis by Tim Berger.
        e. g. German SRMA 2016: 3 x 15 MHz in 900MHz and 4 x 15 MHz in 1800 MHz.
        boni: {'band': valuation_for_package}"""
        AdditivePayoffSelector.__init__(self, bidder_id, items, valuations, limits)
        self.non_additive_boni = non_additive_boni
        self.hierarchy_flattened = hierarchy_flattened
        self._addBoni()
        self._preemptive_epsilon_bundle_selection()

    def _addBoni(self):
        if not self.non_additive_boni:
            return

        m, objective = self._model, self._objective

        # produce boni var
        bonus_vars = []
        bands_with_boni = ['900', '1800']
        for band in bands_with_boni:
            bonus_vars.append(m.addVar(vtype=grb.GRB.BINARY, name='b_var_%s' % band))
        m.update()

        # add constraints for when to activate boni
        boni_bounds = (3, 4)  # 3 x 15 MHz in 900MHz, 4 x 15 MHz in 1800 MHz
        for band, boni_bound, b_var in zip(bands_with_boni, boni_bounds, bonus_vars):
            m.addConstr(
                grb.quicksum(a for (i, a) in zip(self.items, self._ass_vars) if
                             i.band == band) >= boni_bound * b_var,
                name="bonus_%s" % band)

        # add boni bv900 * V900_3 + b1800 * V1800_4 to objective function
        boni_val = []
        for band in bands_with_boni:
            boni_val.append(self.non_additive_boni[band])
        objective_extra = grb.LinExpr()
        objective_extra += grb.quicksum(
            bonus_ind * val for (bonus_ind, val) in zip(bonus_vars, boni_val))

        objective += objective_extra
        m.update()

    def _preemptive_epsilon_bundle_selection(self):
        """The selector needs to know about the package hierarchy in case it exists.
        Bidders should bid on packages rather single items to combine them to achieve boni if prices allow them to."""
        if self.hierarchy_flattened is None:
            return

        m, objective = self._model, self._objective

        # produce package vars
        package_vars = []
        package_items = []
        for package in self.hierarchy_flattened:
            if not isinstance(package, frozenset):
                continue
            package_vars.append(m.addVar(vtype=grb.GRB.BINARY))
            package_items.append(package)
        m.update()

        # add constraints for package vars
        for p_var, p_items in zip(package_vars, package_items):
            # e.g. ass(700_1) + ass(700_2) >= p_var_700_1_700_2 * 2
            m.addConstr(grb.quicksum(a for (i, a) in zip(self.items, self._ass_vars) if i.name in p_items) >= len(
                p_items) * p_var, name="package_%s" % p_items)

        # update objective function
        objective_extra = grb.LinExpr()
        bonus_bounds_per_band = {'700': 0, '900': 3, '1800': 4}
        bonus_package_indicator = []
        for p_var, p_items in zip(package_vars, package_items):
            # determine band of items
            band = [i.band for i in self.items if i.name in p_items][0]
            # check length and decide whether package is worth boni or not
            bonus_package_indicator.append(1 if len(p_items) == bonus_bounds_per_band[band] else 0)

        objective_extra += grb.quicksum(
            bonus_ind * p_var for (bonus_ind, p_var) in zip(bonus_package_indicator, package_vars))

        objective += self.EPSILON * objective_extra
        m.update()