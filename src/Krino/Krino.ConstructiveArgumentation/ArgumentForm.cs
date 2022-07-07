namespace Krino.ConstructiveArgumentation
{
    public enum ArgumentForm
    {
        /// <summary>
        /// Alfa quadrant
        /// </summary>
        /// <remarks>
        /// e.g.: The suspect was driving fast, because the suspect left a long trace of rubber on the road.
        /// </remarks>
        a_is_X_because_a_is_Y,

        /// <summary>
        /// Beta quadrant
        /// </summary>
        /// <remarks>
        /// e.g.: Cycling on the grass is prohibited, because walking on the grass is prohibited.
        /// </remarks>
        a_is_X_because_b_is_X,


        /// <summary>
        /// Gama quadrant
        /// </summary>
        /// <remarks>
        /// q is true because r is true.
        /// e.g.: He must have gone to the pub, because the interview is cancelled.
        /// </remarks>
        q_is_T_because_r_is_T,


        /// <summary>
        /// Delta quadrant
        /// </summary>
        /// <remarks>
        /// e.g.: We only use 10% of our brain, because Einstein said so.
        /// Which can be reconstructed as We only use 10% of our brain (q) [is true (T)], because [we only use 10% of our brain (q)] was said by Einstein (Z).
        /// </remarks>
        q_is_T_because_q_is_Z
    }
}
