using Krino.Vertical.Utils.Enums;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement
{
    /// <summary>
    /// Declares a structural enum with grammatical attributes for morphemes.
    /// </summary>
    public class StructuralAttributes : EnumRootBase
    {
        /// <summary>
        /// Hiding the constructor.
        /// </summary>
        private StructuralAttributes() { }

        private static StructuralAttributes Instance { get; } = new StructuralAttributes();

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
        /// Indicates it is a non-lexeme morpheme.
        /// </summary>
        public static EnumValue NonLexeme { get; } = new EnumValue(Instance);


        /// <summary>
        /// Returns the grammar character encoded in the provided attributes.
        /// </summary>
        /// <remarks>
        /// If multiple grammar characters are encoded then it returns the first one.
        /// </remarks>
        /// <param name="attributes"></param>
        /// <returns></returns>
        public static GrammarCharacter GetGrammarCharacter(BigInteger attributes)
        {
            if (O.IsIn(attributes))
            {
                return GrammarCharacter.O;
            }
            if (I.IsIn(attributes))
            {
                return GrammarCharacter.I;
            }
            if (A.IsIn(attributes))
            {
                return GrammarCharacter.A;
            }
            if (E.IsIn(attributes))
            {
                return GrammarCharacter.E;
            }
            if (U.IsIn(attributes))
            {
                return GrammarCharacter.U;
            }

            return GrammarCharacter.Epsilon;
        }

        /// <summary>
        /// Returns structural attributes for the provided grammar character.
        /// </summary>
        /// <param name="grammarCharacter"></param>
        /// <returns></returns>
        public static BigInteger GetAttributes(GrammarCharacter grammarCharacter)
        {
            if (grammarCharacter == GrammarCharacter.O)
            {
                return O;
            }
            if (grammarCharacter == GrammarCharacter.I)
            {
                return I;
            }
            if (grammarCharacter == GrammarCharacter.A)
            {
                return A;
            }
            if (grammarCharacter == GrammarCharacter.E)
            {
                return E;
            }
            if (grammarCharacter == GrammarCharacter.U)
            {
                return U;
            }

            return Epsilon;
        }
    }
}
