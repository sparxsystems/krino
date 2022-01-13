using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Structural
{
    public class ClauseAttributes : EnumGroupBase
    {
        public ClauseAttributes(EnumGroupBase parent) : base(parent)
        {
            Declarative = new EnumValue(this);
            Interrogative = new EnumValue(this);
            Imperative = new EnumValue(this);
            Exclamative = new EnumValue(this);

            NounClause = new EnumValue(this);
            AdjectiveClause = new EnumValue(this);
            AdverbialClause = new EnumValue(this);

            Premis = new EnumValue(this);
            Conclusion = new EnumValue(this);
        }

        public EnumValue Declarative { get; }
        public EnumValue Interrogative { get; }
        public EnumValue Imperative { get; }
        public EnumValue Exclamative { get; }

        public EnumValue NounClause { get; }
        public EnumValue AdjectiveClause { get; }
        public EnumValue AdverbialClause { get; }



        public EnumValue Premis { get; }
        public EnumValue Conclusion { get; }
    }
}
