using System;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Semantics
{
    /// <summary>
    /// Attributes used for verbs.
    /// </summary>
    [Flags]
    public enum VerbAttributeTypes
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
        AllValenciesMask = Avalent | Monovalent | Bivalent | Trivalent | Quadrivalent | Pentavalent,
    }
}
