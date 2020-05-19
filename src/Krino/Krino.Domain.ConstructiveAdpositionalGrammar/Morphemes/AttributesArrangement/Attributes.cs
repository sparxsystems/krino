﻿using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural;
using Krino.Vertical.Utils.Enums;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement
{
    /// <summary>
    /// Declares a structural enum with grammatical attributes for morphemes.
    /// </summary>
    public class Attributes : EnumRootBase
    {
        /// <summary>
        /// Hiding the constructor.
        /// </summary>
        private Attributes() { }

        private static Attributes Instance { get; } = new Attributes();

        /// <summary>
        /// Stative
        /// </summary>
        public static StativeAttributes O { get; } = new StativeAttributes(Instance);

        /// <summary>
        /// Verbant
        /// </summary>
        public static VerbantAttributes I { get; } = new VerbantAttributes(Instance);

        /// <summary>
        /// Adjunctive
        /// </summary>
        public static AdjunctiveAttributes A { get; } = new AdjunctiveAttributes(Instance);

        /// <summary>
        /// Circumstantial
        /// </summary>
        public static CircumstantialAttributes E { get; } = new CircumstantialAttributes(Instance);

        /// <summary>
        /// AdPosition
        /// </summary>
        public static AdPositionAttributes U { get; } = new AdPositionAttributes(Instance);

        /// <summary>
        /// Epsilon - i.e. if the adposition does not convey any relation, it is purely syntactic.
        /// </summary>
        public static EnumValue Epsilon { get; } = new EnumValue(Instance);


        /// <summary>
        /// Returns true if the attribute indicates it is a lexeme.
        /// </summary>
        /// <param name="attributes"></param>
        /// <returns></returns>
        public static bool IsLexeme(BigInteger attributes)
        {
            if (O.Lexeme.IsIn(attributes) ||
                I.Lexeme.IsIn(attributes) ||
                A.Lexeme.IsIn(attributes) ||
                E.Lexeme.IsIn(attributes) ||
                U.Lexeme.IsIn(attributes))
            {
                return true;
            }

            return false;
        }

        public static bool IsPrefix(BigInteger attributes)
        {
            if (O.NonLexeme.NounPrefix.IsIn(attributes) ||
                I.NonLexeme.VerbPrefix.IsIn(attributes) ||
                A.NonLexeme.AdjextivePrefix.IsIn(attributes) ||
                E.NonLexeme.AdverbPrefix.IsIn(attributes))
            {
                return true;
            }

            return false;
        }

        public static bool IsSuffix(BigInteger attributes)
        {
            if (O.NonLexeme.NounSuffix.IsIn(attributes) ||
                I.NonLexeme.VerbSuffix.IsIn(attributes) ||
                A.NonLexeme.AdjectiveSuffix.IsIn(attributes) ||
                E.NonLexeme.AdverbSuffix.IsIn(attributes))
            {
                return true;
            }

            return false;
        }
    }
}
