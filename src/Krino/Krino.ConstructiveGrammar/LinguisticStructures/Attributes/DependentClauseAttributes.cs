using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes
{
    public class DependentClauseAttributes : EnumGroupBase
    {
        public DependentClauseAttributes(EnumGroupBase parent) : base(parent)
        {
            NounClause = new EnumValue(this);
            AdjectiveClause = new EnumValue(this);
            AdverbialClause = new EnumValue(this);
        }

        public EnumValue NounClause { get; }
        public EnumValue AdjectiveClause { get; }
        public EnumValue AdverbialClause { get; }
    }
}
