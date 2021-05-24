using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public class StructureAttributes : EnumRootBase
    {
        /// <summary>
        /// Hiding the constructor.
        /// </summary>
        private StructureAttributes() { }

        private static StructureAttributes Instance { get; } = new StructureAttributes();

        public static EnumValue Subject { get; } = new EnumValue(Instance);
        public static EnumValue Predicate { get; } = new EnumValue(Instance);
    }
}
