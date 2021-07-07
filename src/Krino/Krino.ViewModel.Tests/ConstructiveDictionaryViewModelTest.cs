using NUnit.Framework;

namespace Krino.ViewModel.Tests
{
    [TestFixture]
    public class ConstructiveDictionaryViewModelTest
    {
        [Test]
        public void GetAdTreeVisualization()
        {
            var viewModel = new ConstructiveDictionaryViewModel();
            var adTrees = viewModel.GetAdTree("I will start .");
            var results = viewModel.GetAdTreeVisualization(adTrees[0]);
        }
    }
}