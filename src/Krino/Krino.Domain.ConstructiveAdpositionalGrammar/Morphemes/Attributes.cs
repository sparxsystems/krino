using System;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    /// <summary>
    /// Defines attributes the morphem can have.
    /// </summary>
    [Flags]
    public enum Attributes
    {
        None = 0,

        /// <summary>
        /// If stative does the verbant.
        /// </summary>
        Unergative = 1,

        /// <summary>
        /// Verbant happens to the actant (stative).
        /// </summary>
        Unaccusative = 2,

        /// <summary>
        /// Adjective that comes before the noun (e.g. little baby).
        /// </summary>
        AttributiveAdjective = 4,

        /// <summary>
        /// Adjective that comes after the verb (e.g. I was happy).
        /// </summary>
        PredicativeAdjective = 8,

        /// <summary>
        /// Valency 0.
        /// </summary>
        Avalent = 16,

        /// <summary>
        /// Valency 1.
        /// </summary>
        Monovalent = 32,

        /// <summary>
        /// Valency 2.
        /// </summary>
        Bivalent = 64,

        /// <summary>
        /// Valency 3.
        /// </summary>
        Trivalent = 128,

        /// <summary>
        /// Valency 4.
        /// </summary>
        Quadrivalent = 256,

        /// <summary>
        /// Valency 5.
        /// </summary>
        Pentavalent = 512,

        /// <summary>
        /// Helper used to find if any valency is specified.
        /// </summary>
        AllValencies = Avalent | Monovalent | Bivalent | Trivalent | Quadrivalent | Pentavalent,
    }
}
