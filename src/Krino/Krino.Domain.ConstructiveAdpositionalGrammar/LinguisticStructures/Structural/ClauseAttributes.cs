using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Structural
{
    public class ClauseAttributes : EnumGroupBase
    {
        public ClauseAttributes(EnumGroupBase parent) : base(parent)
        {
            Independent = new EnumValue(this);
            Dependent = new EnumValue(this);
            Premis = new EnumValue(this);
            Conclusion = new EnumValue(this);
        }

        public EnumValue Independent { get; }
        public EnumValue Dependent { get; }

        public EnumValue Premis { get; }
        public EnumValue Conclusion { get; }
    }
}
