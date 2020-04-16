namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    /// <summary>
    /// Types of adpositions.
    /// </summary>
    public enum AdpositionType
    {
        /// <summary>
        /// This is not an adposition.
        /// </summary>
        None,

        /// <summary>
        /// Adposition filling a structural position.
        /// </summary>
        /// <remarks>
        /// E.g. adposition linking a stative and a verb on a particular valency position.
        /// </remarks>
        Structural,

        /// <summary>
        /// Conjunction.
        /// </summary>
        Conjunction,

        /// <summary>
        /// Preposition.
        /// </summary>
        Preposition,

        /// <summary>
        /// Postpostion.
        /// </summary>
        /// <remarks>
        /// It seems the only english postposition is the word ego.
        /// E.g. Three years ego.
        /// </remarks>
        Postposition,


        InPosition,
    }

}
