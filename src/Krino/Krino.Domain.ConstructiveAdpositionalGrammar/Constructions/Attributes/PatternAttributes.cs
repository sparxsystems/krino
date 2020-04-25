using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Attributes
{
    /// <summary>
    /// Structured attributes of patterns.
    /// </summary>
    public class PatternAttributes : EnumRootBase
    {
        /// <summary>
        /// Hidden constructor.
        /// </summary>
        private PatternAttributes() { }

        private static PatternAttributes Instance { get; } = new PatternAttributes();


        /// <summary>
        /// Indicates which valancy position is saturated.
        /// </summary>
        public static SaturatedValencyPosition ValencyPosition { get; } = new SaturatedValencyPosition(Instance);

        /// <summary>
        /// Indicates the adposition is correlative.
        /// </summary>
        public static Correlative CorrelativeAdposition { get; } = new Correlative(Instance);

        /// <summary>
        /// Indicates the place which is substituted with the correlation.
        /// </summary>
        public static Correlative CorrelativeSubstitute { get; } = new Correlative(Instance);


    }
}
