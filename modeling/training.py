import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import Dataset, DataLoader
import numpy as np
import pandas as pd

from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score, confusion_matrix
from sklearn.decomposition import PCA


DATA_TRAIN = "../Data/full_train.npy"
DATA_VALID = "../Data/full_valid.npy"

NAME="full_d32_l1-e5_shuffled"

SHOULD_SHUFFLE = True
NUM_EPOCHS = 20
MODEL_SIZE = 32
INPUT_DIM = 82
LEARNING_RATE = 1e-5
NHEAD = 4 # if not labeled on file, = 4
NUM_LAYERS = 2 # if not labeled on file, = 2
WEIGHT_DECAY = 0
DROPOUT = 0

print("HYPERPARAMETERS")
print(f"NUM_EPOCHS: {NUM_EPOCHS}")
print(f"MODEL_SIZE: {MODEL_SIZE}")
print(f"INPUT_DIM: {INPUT_DIM}")
print(f"LEARNING_RATE: {LEARNING_RATE}")
print(f"WEIGHT_DECAY: {WEIGHT_DECAY}")
print(f"DROPOUT: {DROPOUT}")
print(f"NHEAD: {NHEAD}")
print(f"NUM_LAYERS: {NUM_LAYERS}")


class Transformer(nn.Module):
    def __init__(self, input_dim=INPUT_DIM, d_model=MODEL_SIZE,
                 nhead=NHEAD, num_layers=NUM_LAYERS, num_classes=2):
        super().__init__()

        # Linear embedding from 80 to d
        self.input_projection = nn.Linear(input_dim, d_model)

        # Encoder
        encoder_layer = nn.TransformerEncoderLayer(d_model=d_model, nhead=nhead, batch_first=True)
        self.transformer_encoder = nn.TransformerEncoder(encoder_layer, num_layers=num_layers)
        self.dropout = nn.Dropout(p=DROPOUT)

        # Classification head
        self.classifier = nn.Linear(d_model, num_classes)

    def forward(self, x):
        batch_size, seq_len, _ = x.shape
        x = self.input_projection(x)
        x = x.permute(1, 0, 2)
        x = self.transformer_encoder(x)
        x = x[-1, :, :]
        logits = self.classifier(x)
        return logits


class NeuralDataset(Dataset):
    def __init__(self, X, y):
        self.X = torch.from_numpy(X).float()
        self.y = torch.from_numpy(y).long()

    def __len__(self):
        return len(self.X)

    def __getitem__(self, idx):
        return self.X[idx], self.y[idx]


def train_one_epoch(loader, model, device, optimizer, criterion):
    model.train()
    total_loss = 0
    correct = 0
    total = 0

    for X_batch, y_batch in loader:
        X_batch, y_batch = X_batch.to(device), y_batch.to(device)

        optimizer.zero_grad()
        logits = model(X_batch)
        loss = criterion(logits, y_batch)
        loss.backward()
        optimizer.step()

        total_loss += loss.item() * X_batch.size(0)
        preds = logits.argmax(dim=1)
        correct += (preds == y_batch).sum().item()
        total += X_batch.size(0)

    avg_loss = total_loss / total
    accuracy = correct / total
    return avg_loss, accuracy


def validate_one_epoch(loader, model, device, optimizer, criterion):
    model.eval()
    total_loss = 0
    correct = 0
    total = 0

    with torch.no_grad():
        for X_batch, y_batch in loader:
            X_batch, y_batch = X_batch.to(device), y_batch.to(device)
            logits = model(X_batch)
            loss = criterion(logits, y_batch)
            total_loss += loss.item() * X_batch.size(0)

            preds = logits.argmax(dim=1)
            correct += (preds == y_batch).sum().item()
            total += X_batch.size(0)
    avg_loss = total_loss / total
    accuracy = correct / total
    return avg_loss, accuracy

def logistic_regression(train_data, train_labels, valid_data, valid_labels):
    l_train_data = train_data.reshape(train_data.shape[0], -1)
    l_valid_data = valid_data.reshape(valid_data.shape[0], -1)

    pca = PCA(n_components=100)
    l_train_data = pca.fit_transform(l_train_data)
    l_valid_data = pca.transform(l_valid_data)

    model = LogisticRegression(solver='lbfgs', max_iter=10000, C=0.1)
    model.fit(l_train_data, train_labels)
    y_pred = model.predict(l_valid_data)
    acc = accuracy_score(valid_labels, y_pred)
    print("Logistic regression accuracy:", acc)

def training_pipeline(should_shuffle=False):
    print(NAME)

    # LOAD TRAINING SET
    train_npy = np.load(DATA_TRAIN)
    train_labels = train_npy[:, 0, 0]
    train_labels = (train_labels == 1).astype(np.int64)
    train_data = train_npy[:, :, 1:]
    train_idx = np.where(~np.isnan(train_data).any(axis=(1,2)))[0]
    train_data = train_data[train_idx]
    train_labels = train_labels[train_idx]
    if (should_shuffle):
        np.random.shuffle(train_labels)
    train_ds = NeuralDataset(train_data, train_labels)


    # LOAD VALIDATION SET
    valid_npy = np.load(DATA_VALID)
    valid_labels = valid_npy[:, 0, 0]
    valid_labels = (valid_labels == 1).astype(np.int64)
    valid_data = valid_npy[:, :, 1:]
    valid_idx = np.where(~np.isnan(valid_data).any(axis=(1,2)))[0]
    valid_data = valid_data[valid_idx]
    valid_labels = valid_labels[valid_idx]
    if (should_shuffle):
        np.random.shuffle(valid_labels)
    valid_ds = NeuralDataset(valid_data, valid_labels)


    # FIND OVERLAPS BETWEEN TRAINING/VALIDATION SET
    print("Filtered train_data shape:", train_data.shape)
    print("Filtered valid_data shape:", valid_data.shape)

    train_flat = train_npy.reshape(train_npy.shape[0], -1)
    valid_flat = valid_npy.reshape(valid_npy.shape[0], -1)
    train_set = set(map(tuple, train_flat))
    valid_set = set(map(tuple, valid_flat))

    exact_matches = sum(1 for row in train_flat if any(np.array_equal(row, v_row)
                                                       for v_row in valid_flat))
    print(f"Exact matches between train and validation: {exact_matches}")

    overlap = train_set.intersection(valid_set)
    if (len(overlap) == len(train_set)):
        print("Warning: All training samples appear in the validation set!")
    print("Number of overlapping samples:", len(overlap))

    """
    # CHECK FEATURE CORRELATION WITH LABELS
    df_train = pd.DataFrame(train_data.reshape(train_data.shape[0], -1))
    df_train['label'] = train_labels

    print("Feature correlation with labels:")
    df_corr = df_train.corr()['label'].sort_values(ascending=False)
    print(df_corr.head(20))
    """

    # ENSURE EQUAL CLASS DISTRIBUTION
    print("Class distribution training:", np.bincount(train_labels))
    print("Class distribution validation:", np.bincount(valid_labels))

    train_loader = DataLoader(train_ds, batch_size=32, shuffle=True)
    valid_loader = DataLoader(valid_ds, batch_size=32, shuffle=False)


    # CONFIGURE TOOLS
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    model = Transformer(
            input_dim=INPUT_DIM, d_model=MODEL_SIZE, nhead=NHEAD,
            num_layers=NUM_LAYERS, num_classes=2
            ).to(device)
    criterion = nn.CrossEntropyLoss()
    optimizer = optim.Adam(model.parameters(), lr=LEARNING_RATE, weight_decay=WEIGHT_DECAY)


    # TRY LOGISTIC REGRESSION AS A BASELINE
    logistic_regression(train_data, train_labels, valid_data, valid_labels)

    """
    # 3a. Some debug lives here
    #np.set_printoptions(threshold=np.inf)
    print(train_data.shape)

    print("Train data NaN count:", np.isnan(train_data).sum())
    print("Valid data NaN count:", np.isnan(valid_data).sum())
    # Train data 1968 NaN identified; Valid data 351 NaN identified. Time to go hunting
    train_nan = (np.argwhere(np.isnan(train_data)))
    print(train_nan)
    #print(train_data[train_nan])
    """

    # TRAIN MODEL
    epoch_info = []

    num_epochs = NUM_EPOCHS
    for epoch in range(num_epochs):
        train_loss, train_acc = train_one_epoch(
                train_loader, model, device, optimizer, criterion)
        valid_loss, valid_acc = validate_one_epoch(
                valid_loader, model, device, optimizer, criterion)
        print(f"Epoch {epoch+1}/{num_epochs} "
              f"Train Loss: {train_loss:.4f} Acc: {train_acc:.4f} "
              f"Valid Loss: {valid_loss:.4f} Acc: {valid_acc:.4f}")

        epoch_info.append({
            'epoch': epoch+1,
            'train_loss': train_loss,
            'train_acc': train_acc,
            'valid_loss': valid_loss,
            'valid_acc': valid_acc
        })

    df_epochs = pd.DataFrame(epoch_info)


    # SAVE MODEL AND DATA
    torch.save(model.state_dict(), "Models/model_"+NAME+".pth")
    df_epochs.to_csv("Data/data_"+NAME+".csv", index=False)


def main():
    training_pipeline(SHOULD_SHUFFLE)

if __name__ == "__main__":
    main()
