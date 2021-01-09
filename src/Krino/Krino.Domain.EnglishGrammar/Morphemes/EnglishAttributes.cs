using Krino.Domain.EnglishGrammar.Morphemes.Structural;
using Krino.Vertical.Utils.Enums;
using System.Numerics;

namespace Krino.Domain.EnglishGrammar.Morphemes
{
    /// <summary>
    /// Declares a structural enum with grammatical attributes for morphemes.
    /// </summary>
    public class EnglishAttributes : EnumRootBase
    {
        /// <summary>
        /// Hiding the constructor.
        /// </summary>
        private EnglishAttributes() { }

        private static EnglishAttributes Instance { get; } = new EnglishAttributes();

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

        public static bool IsNonLexeme(BigInteger attributes)
        {
            if (O.NonLexeme.IsIn(attributes) ||
                I.NonLexeme.IsIn(attributes) ||
                A.NonLexeme.IsIn(attributes) ||
                E.NonLexeme.IsIn(attributes) ||
                U.NonLexeme.IsIn(attributes))
            {
                return true;
            }

            return false;
        }

        public static bool IsPrefix(BigInteger attributes)
        {
            if (O.NonLexeme.Prefix.IsIn(attributes) ||
                I.NonLexeme.Prefix.IsIn(attributes) ||
                A.NonLexeme.Prefix.IsIn(attributes) ||
                E.NonLexeme.AdverbPrefix.IsIn(attributes))
            {
                return true;
            }

            return false;
        }

        public static bool IsSuffix(BigInteger attributes)
        {
            if (O.NonLexeme.Suffix.IsIn(attributes) ||
                I.NonLexeme.Suffix.IsIn(attributes) ||
                A.NonLexeme.Suffix.IsIn(attributes) ||
                E.NonLexeme.AdverbSuffix.IsIn(attributes))
            {
                return true;
            }

            return false;
        }
    }
}
