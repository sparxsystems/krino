namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    /// <summary>
    /// Types of grammar characters.
    /// </summary>
    public enum GrammarCharacter
    {
        /// <summary>
        /// Epsilon - zero-marked adposition.
        /// </summary>
        /// <remarks>
        /// This grammar character is used for adpositions which are syntactic and do not have any morpheme.
        /// </remarks>
        e,

        /// <summary>
        /// Verbant
        /// </summary>
        I,

        /// <summary>
        /// Stative
        /// </summary>
        O,

        /// <summary>
        /// Adjunctive
        /// </summary>
        A,

        /// <summary>
        /// Circumstantial
        /// </summary>
        E,

        /// <summary>
        /// Adposition
        /// </summary>
        /// <remarks>
        /// This grammar character is used for adpositions which contain a morpheme linking other grammar characters
        /// into complex structures.
        /// E.g. conjunctions or punctation marks.
        /// </remarks>
        U
    }
}
