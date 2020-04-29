using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement
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
        public static ValencyPositionAttributes ValencyPosition { get; } = new ValencyPositionAttributes(Instance);

        /// <summary>
        /// Indicates the adposition is correlative.
        /// </summary>
        public static CorrelativeAttributes CorrelativeAdposition { get; } = new CorrelativeAttributes(Instance);

        /// <summary>
        /// Indicates the place which is substituted with the correlation.
        /// </summary>
        public static CorrelativeAttributes CorrelativeSubstitute { get; } = new CorrelativeAttributes(Instance);


    }
}
