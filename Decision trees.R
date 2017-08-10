#======================================================================================
# Pruning with prior probabilities
#======================================================================================

# tree_prior is loaded in your workspace

# Plot the cross-validated error rate as a function of the complexity parameter
plotcp(tree_prior)

# Use printcp() to identify for which complexity parameter the cross-validated error rate is minimized
printcp(tree_prior)

# Create an index for of the row with the minimum xerror
index <- which.min(tree_prior$cptable[, "xerror"])

# Create tree_min
tree_min <- tree_prior$cptable[index, "CP"]

#  Prune the tree using tree_min
ptree_prior <- prune(tree_prior, cp = tree_min)

# Use prp() to plot the pruned tree
prp(ptree_prior)


#======================================================================================
# Loss Matrix Pruning
#======================================================================================

# set a seed and run the code to construct the tree with the loss matrix again
set.seed(345)
tree_loss_matrix  <- rpart(loan_status ~ ., method = "class", data = training_set,
                           parms = list(loss=matrix(c(0, 10, 1, 0), ncol = 2)),
                           control = rpart.control(cp = 0.001))

# Plot the cross-validated error rate as a function of the complexity parameter
plotcp(tree_loss_matrix)

# Prune the tree using cp = 0.0012788
ptree_loss_matrix <- prune(tree_loss_matrix, cp = 0.0012788)

# Use prp() and argument extra = 1 to plot the pruned tree
prp(ptree_loss_matrix, extra = 1)

#======================================================================================
# Weighted tree
#======================================================================================

# set a seed and run the code to obtain a tree using weights, minsplit and minbucket
set.seed(345)
tree_weights <- rpart(loan_status ~ ., method = "class", weights = case_weights,
                      data = training_set,
                      control = rpart.control(minsplit = 5, minbucket = 2, cp = 0.001))

# Plot the cross-validated error rate for a changing cp
plotcp(tree_weights)

#======================================================================================
# Evaluate models
#======================================================================================

# Make predictions for each of the pruned trees using the test set.
pred_undersample <- predict(ptree_undersample, newdata = test_set,  type = "class")
pred_prior <- predict(ptree_prior, newdata = test_set, type = "class")
pred_loss_matrix <- predict(ptree_loss_matrix, newdata = test_set, type = "class")
pred_weights <- predict(ptree_weights, newdata = test_set, type = "class")

# construct confusion matrices using the predictions.
confmat_undersample <- table(test_set$loan_status, pred_undersample)
confmat_prior <- table(test_set$loan_status, pred_prior)
confmat_loss_matrix <- table(test_set$loan_status, pred_loss_matrix)
confmat_weights <- table(test_set$loan_status, pred_weights)

# Compute the accuracies
acc_undersample <- sum(diag(confmat_undersample)) / nrow(test_set)
acc_prior <- sum(diag(confmat_prior)) / nrow(test_set)
acc_loss_matrix <- sum(diag(confmat_loss_matrix)) / nrow(test_set)
acc_weights <- sum(diag(confmat_weights)) / nrow(test_set)
