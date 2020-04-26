using Krino.Vertical.Utils.Enums;

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
        /// Epsilon - i.e. without the grammar character.
        /// </summary>
        public static EnumValue Epsilon { get; } = new EnumValue(Instance);


        /// <summary>
        /// Returns the grammar character encoded in the provided attributes.
        /// </summary>
        /// <remarks>
        /// If multiple grammar characters are encoded then it returns the first one.
        /// </remarks>
        /// <param name="attributes"></param>
        /// <returns></returns>
        public static GrammarCharacter GetGrammarCharacter(ulong attributes)
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
    }
}
