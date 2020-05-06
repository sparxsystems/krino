using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural;
using Krino.Vertical.Utils.Enums;

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
        /// Indicates it is a non-lexeme morpheme.
        /// </summary>
        public static EnumValue NonLexeme { get; } = new EnumValue(Instance);

        
    }
}
