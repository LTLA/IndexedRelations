.standardize_featureSets <- function(x, objects, clean=FALSE) 
# Utility to standardize the featureSets of different IndexedRelations objects.
# This allows them to be compared, merged, etc.
{
    # Checking that objects are even mergeable.
    .verify_identity(x, objects, function(x) lapply(featureSets(x), class),
        "'featureSets' should have the same classes for all objects")

    .verify_identity(x, objects, function(x) ncol(partners(x)),
        "'partners' should have the same 'ncol' for all objects")

    .verify_identity(x, objects, mapping, "'mapping' should be the same for all objects")

    # 'clean' means that each of the feature sets need to be sorted and unique.
    # Provides some useful structure for downstream steps.
    if (clean) {
        x <- .clean_featureSets(x)
        objects <- lapply(objects, .clean_featureSets)
    }

    # Reindexing the features to the same set.
    nobjects <- length(objects)
    ref.features <- featureSets(x)

    for (i in seq_len(nobjects)) {
        cur.features <- featureSets(objects[[i]])
        cur.partners <- partners(objects[[i]])
        cur.map <- mapping(objects[[i]])

        remap <- vector("list", length(cur.features))
        for (j in seq_along(remap)) {
            out <- .combine_features(cur.features[[j]], ref.features[[j]])
            ref.features[[j]] <- out$ref
            remap[[j]] <- out$id
        }

        for (k in seq_len(ncol(cur.partners))) {
            ftype <- cur.map[k]
            cur.partners[,k] <- remap[[ftype]][cur.partners[,k]]
        }
        partners(objects[[i]]) <- cur.partners
    }

    featureSets(x) <- ref.features
    for (i in seq_len(nobjects)) {
        featureSets(objects[[i]]) <- ref.features
    }

    list(x=x, objects=objects)
}

.verify_identity <- function(x, objects, FUN, msg) {
    ref <- FUN(x)
    others <- lapply(objects, FUN)
    if (!all(vapply(others, identical, y=ref, FUN.VALUE=TRUE))) {
        stop(msg)
    }
    NULL
}

.clean_featureSets <- function(x) {
    fsets <- featureSets(x)
    remap <- vector("list", length(fsets))

    for (i in seq_along(fsets)) {
        new.f <- unique(sort(fsets[[i]]))
        remap[[i]] <- match(fsets[[i]], new.f)
        fsets[[i]] <- new.f
    }

    psets <- partners(x)
    cur.map <- mapping(x)
    for (k in seq_len(ncol(psets))) {
        ftype <- cur.map[k]
        psets[,k] <- remap[[ftype]][psets[,k]]
    }

    featureSets(x) <- fsets
    partners(x) <- psets
    x
}

